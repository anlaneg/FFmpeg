/*
 * Format register and lookup
 * Copyright (c) 2000, 2001, 2002 Fabrice Bellard
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "config_components.h"

#include "libavutil/avstring.h"
#include "libavutil/mem.h"
#include "libavutil/opt.h"

#include "avio_internal.h"
#include "avformat.h"
#include "demux.h"
#include "id3v2.h"
#include "internal.h"
#include "url.h"


/**
 * @file
 * Format register and lookup
 */

int av_match_ext(const char *filename, const char *extensions)
{
    const char *ext;

    if (!filename)
        return 0;/*文件名称为空,不能匹配*/

    ext = strrchr(filename, '.');
    if (ext)
    	/*获得文件后缀*/
        return av_match_name(ext + 1, extensions);
    return 0;
}

int ff_match_url_ext(const char *url, const char *extensions)
{
    const char *ext;
    URLComponents uc;
    int ret;
    char scratchpad[128];

    if (!url)
        return 0;

    ret = ff_url_decompose(&uc, url, NULL);
    if (ret < 0 || !URL_COMPONENT_HAVE(uc, scheme))
        return ret;
    for (ext = uc.query; *ext != '.' && ext > uc.path; ext--)
        ;

    if (*ext != '.')
        return 0;
    if (uc.query - ext > sizeof(scratchpad))
        return AVERROR(ENOMEM); //not enough memory in our scratchpad
    av_strlcpy(scratchpad, ext + 1, uc.query - ext);

    return av_match_name(scratchpad, extensions);
}

const AVOutputFormat *av_guess_format(const char *short_name, const char *filename,
                                      const char *mime_type)
{
    const AVOutputFormat *fmt = NULL;
    const AVOutputFormat *fmt_found = NULL;
    void *i = 0;
    int score_max, score;

    /* specific test for image sequences */
#if CONFIG_IMAGE2_MUXER
    if (!short_name && filename &&
        av_filename_number_test(filename) &&
        ff_guess_image2_codec(filename) != AV_CODEC_ID_NONE) {
        return av_guess_format("image2", NULL, NULL);
    }
#endif
    /* Find the proper file type. */
    score_max = 0;
    while ((fmt = av_muxer_iterate(&i))) {
        if (fmt->flags & AVFMT_EXPERIMENTAL && !short_name)
            continue;
        score = 0;
        if (fmt->name && short_name && av_match_name(short_name, fmt->name))
            score += 100;
        if (fmt->mime_type && mime_type && !strcmp(fmt->mime_type, mime_type))
            score += 10;
        if (filename && fmt->extensions &&
            av_match_ext(filename, fmt->extensions)) {
            score += 5;
        }
        if (score > score_max) {
            score_max = score;
            fmt_found = fmt;
        }
    }
    return fmt_found;
}

enum AVCodecID av_guess_codec(const AVOutputFormat *fmt, const char *short_name,
                              const char *filename, const char *mime_type,
                              enum AVMediaType type)
{
    if (av_match_name("segment", fmt->name) || av_match_name("ssegment", fmt->name)) {
        const AVOutputFormat *fmt2 = av_guess_format(NULL, filename, NULL);
        if (fmt2)
            fmt = fmt2;
    }

    if (type == AVMEDIA_TYPE_VIDEO) {
        enum AVCodecID codec_id = AV_CODEC_ID_NONE;

#if CONFIG_IMAGE2_MUXER || CONFIG_IMAGE2PIPE_MUXER
        if (!strcmp(fmt->name, "image2") || !strcmp(fmt->name, "image2pipe")) {
            codec_id = ff_guess_image2_codec(filename);
        }
#endif
        if (codec_id == AV_CODEC_ID_NONE)
            codec_id = fmt->video_codec;
        return codec_id;
    } else if (type == AVMEDIA_TYPE_AUDIO)
        return fmt->audio_codec;
    else if (type == AVMEDIA_TYPE_SUBTITLE)
        return fmt->subtitle_codec;
    else
        return AV_CODEC_ID_NONE;
}

const AVInputFormat *av_find_input_format(const char *short_name)
{
    const AVInputFormat *fmt = NULL;
    void *i = 0;
    /*遍历并返回short_name对应的fmt*/
    while ((fmt = av_demuxer_iterate(&i)))
        if (av_match_name(short_name, fmt->name))
            return fmt;/*名称匹配，返回格式*/
    return NULL;
}

const AVInputFormat *av_probe_input_format3(const AVProbeData *pd,
                                            int is_opened/*文件是否已打开*/, int *score_ret/*出参,得分情况*/)
{
    AVProbeData lpd = *pd;
    const AVInputFormat *fmt1 = NULL;
    const AVInputFormat *fmt = NULL;
    int score, score_max = 0;
    void *i = 0;
    const static uint8_t zerobuffer[AVPROBE_PADDING_SIZE];
    enum nodat {
        NO_ID3,
        ID3_ALMOST_GREATER_PROBE,
        ID3_GREATER_PROBE,
        ID3_GREATER_MAX_PROBE,
    } nodat = NO_ID3;

    if (!lpd.buf)
    	/*为指定buf,为其指定临时buf,此时buf_size为0*/
        lpd.buf = (unsigned char *) zerobuffer;

    if (lpd.buf_size > 10 && ff_id3v2_match(lpd.buf, ID3v2_DEFAULT_MAGIC)) {
    	/*匹配ID3v2_DEFAULT_MAGIC*/
        int id3len = ff_id3v2_tag_len(lpd.buf);
        if (lpd.buf_size > id3len + 16) {
            if (lpd.buf_size < 2LL*id3len + 16)
                nodat = ID3_ALMOST_GREATER_PROBE;
            lpd.buf      += id3len;
            lpd.buf_size -= id3len;
        } else if (id3len >= PROBE_BUF_MAX) {
            nodat = ID3_GREATER_MAX_PROBE;
        } else
            nodat = ID3_GREATER_PROBE;
    }

    /*遍历所有demuxer,检查匹配情况*/
    while ((fmt1 = av_demuxer_iterate(&i))) {
        if (fmt1->flags & AVFMT_EXPERIMENTAL)
            continue;/*忽略掉试验性的*/
        if (!is_opened == !(fmt1->flags & AVFMT_NOFILE) && strcmp(fmt1->name, "image2"))
        	/*没有打开文件且FMT也不需要文件,或者打开了文件且FMT也需要文件,且文件名称不等于image2会continue
        	 * 即当FMT1->NAME名称为"IMAGE2"时此选项才可能通过*/
            continue;
        score = 0;
        if (ffifmt(fmt1)->read_probe) {
        	/*有此回调,利用read_probe函数来探测文件格式*/
            score = ffifmt(fmt1)->read_probe(&lpd);
            if (score)
            	/*当得分不0时,显示得分*/
                av_log(NULL, AV_LOG_TRACE, "Probing %s score:%d size:%d\n", fmt1->name, score, lpd.buf_size);
            if (fmt1->extensions && av_match_ext(lpd.filename, fmt1->extensions)) {
            	/*此格式指定了后缀名称,且后缀名称匹配成功*/
                switch (nodat) {
                case NO_ID3:
                    score = FFMAX(score, 1);
                    break;
                case ID3_GREATER_PROBE:
                case ID3_ALMOST_GREATER_PROBE:
                    score = FFMAX(score, AVPROBE_SCORE_EXTENSION / 2 - 1);
                    break;
                case ID3_GREATER_MAX_PROBE:
                    score = FFMAX(score, AVPROBE_SCORE_EXTENSION);
                    break;
                }
            }
        } else if (fmt1->extensions) {
        	/*利用后缀来检查*/
            if (av_match_ext(lpd.filename, fmt1->extensions))
                score = AVPROBE_SCORE_EXTENSION;
        }

        /*尝试利用mime_type进行匹配*/
        if (av_match_name(lpd.mime_type, fmt1->mime_type)) {
            int old_score = score;
            score += AVPROBE_SCORE_MIME_BONUS;
        	/*利用mime_type类型来检测*/
            if (score > AVPROBE_SCORE_MAX) score = AVPROBE_SCORE_MAX;
            av_log(NULL, AV_LOG_DEBUG, "Probing %s score:%d increased to %d due to MIME type\n", fmt1->name, old_score, score);
        }

        /*检查是否最优*/
        if (score > score_max) {
            score_max = score;
            fmt       = fmt1;/*选择得分最高的FMT*/
        } else if (score == score_max)
            fmt = NULL;
    }
    if (nodat == ID3_GREATER_PROBE)
        score_max = FFMIN(AVPROBE_SCORE_EXTENSION / 2 - 1, score_max);
    *score_ret = score_max;

    return fmt;/*返回命中的格式,例如:ff_mov_demuxer*/
}

/*探测输入文件格式*/
const AVInputFormat *av_probe_input_format2(const AVProbeData *pd,
                                            int is_opened/*文件是否已打开*/, int *score_max/*出参,此格式得分情况*/)
{
    int score_ret;
    const AVInputFormat *fmt = av_probe_input_format3(pd, is_opened, &score_ret);
    if (score_ret > *score_max) {
        *score_max = score_ret;
        return fmt;/*使用获得文件对应的format*/
    } else
        return NULL;
}

const AVInputFormat *av_probe_input_format(const AVProbeData *pd, int is_opened)
{
    int score = 0;
    return av_probe_input_format2(pd, is_opened, &score);
}

/*检测文件格式*/
int av_probe_input_buffer2(AVIOContext *pb, const AVInputFormat **fmt/*出参,文件格式*/,
                           const char *filename/*文件名称*/, void *logctx,
                           unsigned int offset, unsigned int max_probe_size/*为检测文件可格式最多PROBE多少字节*/)
{
    AVProbeData pd = { filename ? filename : "" };
    uint8_t *buf = NULL;
    int ret = 0, probe_size, buf_offset = 0;
    int score = 0;
    int ret2;
    int eof = 0;

    if (!max_probe_size)
        max_probe_size = PROBE_BUF_MAX;/*使用默认BUFF大小*/
    else if (max_probe_size < PROBE_BUF_MIN) {
    	/*指定的PROBE SIZE过小*/
        av_log(logctx, AV_LOG_ERROR,
               "Specified probe size value %u cannot be < %u\n", max_probe_size, PROBE_BUF_MIN);
        return AVERROR(EINVAL);
    }

    if (offset >= max_probe_size)
        return AVERROR(EINVAL);/*offset有误*/

    if (pb->av_class) {
        uint8_t *mime_type_opt = NULL;
        char *semi;
        av_opt_get(pb, "mime_type", AV_OPT_SEARCH_CHILDREN, &mime_type_opt);/*取mime_type配置*/
        pd.mime_type = (const char *)mime_type_opt;
        semi = pd.mime_type ? strchr(pd.mime_type, ';') : NULL;
        if (semi) {
            *semi = '\0';
        }
    }

    for (probe_size = PROBE_BUF_MIN/*设置PROBE初值*/; probe_size <= max_probe_size && !*fmt && !eof;
         probe_size = FFMIN(probe_size << 1,
                            FFMAX(max_probe_size, probe_size + 1))) {
        score = probe_size < max_probe_size ? AVPROBE_SCORE_RETRY : 0;

        /* Read probe data. */
        if ((ret = av_reallocp(&buf, probe_size + AVPROBE_PADDING_SIZE)) < 0)/*增大buf*/
            goto fail;/*扩大内存失败*/
        if ((ret = avio_read(pb, buf + buf_offset/*填写位置*/,
                             probe_size - buf_offset/*最大读取长度*/)) < 0) {
        	/*读取时出错了*/
            /* Fail if error was not end of file, otherwise, lower score. */
            if (ret != AVERROR_EOF)
                goto fail;

            score = 0;
            ret   = 0;          /* error was end of file, nothing read */
            eof   = 1;
        }
        buf_offset += ret;/*buf偏移量增加,记录写的位置*/
        if (buf_offset < offset)
            continue;
        pd.buf_size = buf_offset - offset;/*设置读到的数据长度*/
        pd.buf = &buf[offset];/*设置数据起始地址*/

        memset(pd.buf + pd.buf_size, 0, AVPROBE_PADDING_SIZE);/*添加PAD,使之为0*/

        /* Guess file format. */
        *fmt = av_probe_input_format2(&pd, 1/*指明文件已打开*/, &score);/*检测并获得文件格式*/
        if (*fmt) {
            /* This can only be true in the last iteration. */
            if (score <= AVPROBE_SCORE_RETRY) {
                av_log(logctx, AV_LOG_WARNING,
                       "Format %s detected only with low score of %d, "
                       "misdetection possible!\n", (*fmt)->name, score);
            } else
                av_log(logctx, AV_LOG_DEBUG,
                       "Format %s probed with size=%d and score=%d\n",
                       (*fmt)->name, probe_size, score);/*显示探测长度及得分*/
#if 0
            FILE *f = fopen("probestat.tmp", "ab");
            fprintf(f, "probe_size:%d format:%s score:%d filename:%s\n", probe_size, (*fmt)->name, score, filename);
            fclose(f);
#endif
        }
    }

    if (!*fmt)
        ret = AVERROR_INVALIDDATA;

fail:
    /* Rewind. Reuse probe buffer to avoid seeking. */
    ret2 = ffio_rewind_with_probe_data(pb, &buf, buf_offset);/*回退数据,使PB->buffer指向从0位置开始的数据*/
    if (ret >= 0)
        ret = ret2;/*ret未出错,出错情况看ret2,是否归还失败*/

    av_freep(&pd.mime_type);
    return ret < 0 ? ret/*归还失败*/ : score/*归还成功,返回得分*/;
}

int av_probe_input_buffer(AVIOContext *pb, const AVInputFormat **fmt,
                          const char *filename, void *logctx,
                          unsigned int offset, unsigned int max_probe_size)
{
    int ret = av_probe_input_buffer2(pb, fmt, filename, logctx, offset, max_probe_size);
    return ret < 0 ? ret/*出错*/ : 0/*PROBE成功*/;
}
