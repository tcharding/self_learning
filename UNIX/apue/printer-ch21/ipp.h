#ifndef _IPP_H
#define _IPP_H

/*
 * Defines parts of the IPP protocol between the scheduler
 * and the printer.  Based on RFC2911 and RFC2910.
 */

/*
 * Status code classes.
 */
#define STATCLASS_OK(x)     ((x) >= 0x0000 && (x) <= 0x00ff)
#define STATCLASS_INFO(x)   ((x) >= 0x0100 && (x) <= 0x01ff)
#define STATCLASS_REDIR(x)  ((x) >= 0x0300 && (x) <= 0x03ff)
#define STATCLASS_CLIERR(x) ((x) >= 0x0400 && (x) <= 0x04ff)
#define STATCLASS_SRVERR(x) ((x) >= 0x0500 && (x) <= 0x05ff)

/*
 * Status codes. see below for description.
 */
#define STAT_INVALID 0x000
#define STAT_OK           0x0001 
#define STAT_OK_ATTRIGN   0x0002 
#define STAT_OK_ATTRCON   0x0003

				/* client error */
#define STAT_CLI_BADREQ   0x0400 
#define STAT_CLI_FORBID   0x0401 
#define STAT_CLI_NOAUTH   0x0402 
#define STAT_CLI_NOPERM   0x0403 
#define STAT_CLI_NOTPOS   0x0404 
#define STAT_CLI_TIMOUT   0x0405 
#define STAT_CLI_NOTFND   0x0406 
#define STAT_CLI_OBJGONE  0x0407 
#define STAT_CLI_TOOBIG   0x0408 
#define STAT_CLI_TOOLNG   0x0409 
#define STAT_CLI_BADFMT   0x040a 
#define STAT_CLI_NOTSUP   0x040b 
#define STAT_CLI_NOSCHM   0x040c 
#define STAT_CLI_NOCHAR   0x040d 
#define STAT_CLI_ATTRCON  0x040e 
#define STAT_CLI_NOCOMP   0x040f 
#define STAT_CLI_COMPERR  0x0410 
#define STAT_CLI_FMTERR   0x0411 
#define STAT_CLI_ACCERR   0x0412 
				/* server error */
#define STAT_SRV_INTERN   0x0500 
#define STAT_SRV_NOTSUP   0x0501 
#define STAT_SRV_UNAVAIL  0x0502 
#define STAT_SRV_BADVER   0x0503 
#define STAT_SRV_DEVERR   0x0504 
#define STAT_SRV_TMPERR   0x0505 
#define STAT_SRV_REJECT   0x0506 
#define STAT_SRV_TOOBUSY  0x0507 
#define STAT_SRV_CANCEL   0x0508 
#define STAT_SRV_NOMULTI  0x0509

struct err_info {
	int err_code;
	char *err_msg;
} err_tab[] = {
	{ STAT_OK, "success" },
	{ STAT_OK_ATTRIGN, "OK; some attrs ignored" },
	{ STAT_OK_ATTRCON, "OK; some attrs conflicted" },
	{ STAT_CLI_BADREQ, "invalid client request" },
	{ STAT_CLI_FORBID, "request is forbidden" },
	{ STAT_CLI_NOAUTH, "authentication required" },
	{ STAT_CLI_NOPERM, "client not authorized" },
	{ STAT_CLI_NOTPOS, "request not possible" },
	{ STAT_CLI_TIMOUT, "client too slow" },
	{ STAT_CLI_NOTFND, "no object found for URI" },
	{ STAT_CLI_OBJGONE, "object no longer available" },
	{ STAT_CLI_TOOBIG, "requested entity too big" },
	{ STAT_CLI_TOOLNG, "attribute value too large" },
	{ STAT_CLI_BADFMT, "unsupported doc format" },
	{ STAT_CLI_NOTSUP, "attributes not supported" },
	{ STAT_CLI_NOSCHM, "URI scheme not supported" },
	{ STAT_CLI_NOCHAR, "charset not supported" },
	{ STAT_CLI_ATTRCON,"attributes conflicted" },
	{ STAT_CLI_NOCOMP, "compression not supported" },
	{ STAT_CLI_COMPERR, "data can't be decompressed" },
	{ STAT_CLI_FMTERR, "document format error" },
	{ STAT_CLI_ACCERR, "error accessing data" },
	{ STAT_SRV_INTERN, "unexpected internal error" },
	{ STAT_SRV_NOTSUP, "operation not supported" },
	{ STAT_SRV_UNAVAIL,"service unavailable" },
	{ STAT_SRV_BADVER, "version not supported" },
	{ STAT_SRV_DEVERR, "device error" },
	{ STAT_SRV_TMPERR, "temporary error" },
	{ STAT_SRV_REJECT, "server not accepting jobs" },
	{ STAT_SRV_TOOBUSY, "server too busy" },
	{ STAT_SRV_CANCEL, "job has been canceled" },
	{ STAT_SRV_NOMULTI, "multi-doc jobs unsupported" },
	{ STAT_INVALID, NULL }
};

/*
 * Operation IDs
 */
#define OP_PRINT_JOB         0x02
#define OP_PRINT_URI         0x03
#define OP_VALIDATE_JOB      0x04
#define OP_CREATE_JOB        0x05
#define OP_SEND_DOC          0x06
#define OP_SEND_URI          0x07
#define OP_CANCEL_JOB        0x08
#define OP_GET_JOB_ATTR      0x09
#define OP_GET_JOBS          0x0a
#define OP_GET_PRINTER_ATTR  0x0b
#define OP_HOLD_JOB          0x0c
#define OP_RELEASE_JOB       0x0d
#define OP_RESTART_JOB       0x0e
#define OP_PAUSE_PRINTER     0x10
#define OP_RESUME_PRINTER    0x11
#define OP_PURGE_JOBS        0x12

/*
 * Attribute Tags.
 */
#define TAG_OPERATION_ATTR   0x01	/* operation attributes tag */
#define TAG_JOB_ATTR         0x02	/* job attributes tag */
#define TAG_END_OF_ATTR      0x03	/* end of attributes tag */
#define TAG_PRINTER_ATTR     0x04	/* printer attributes tag */
#define TAG_UNSUPP_ATTR      0x05	/* unsupported attributes tag */

/*
 * Value Tags.
 */
#define TAG_UNSUPPORTED      0x10	/* unsupported value */
#define TAG_UNKNOWN          0x12	/* unknown value */
#define TAG_NONE             0x13	/* no value */
#define TAG_INTEGER          0x21	/* integer */
#define TAG_BOOLEAN          0x22	/* boolean */
#define TAG_ENUM             0x23	/* enumeration */
#define TAG_OCTSTR           0x30	/* octetString */
#define TAG_DATETIME         0x31	/* dateTime */
#define TAG_RESOLUTION       0x32	/* resolution */
#define TAG_INTRANGE         0x33	/* rangeOfInteger */
#define TAG_TEXTWLANG        0x35	/* textWithLanguage */
#define TAG_NAMEWLANG        0x36	/* nameWithLanguage */
#define TAG_TEXTWOLANG       0x41	/* textWithoutLanguage */
#define TAG_NAMEWOLANG       0x42	/* nameWithoutLanguage */
#define TAG_KEYWORD          0x44	/* keyword */
#define TAG_URI              0x45	/* URI */
#define TAG_URISCHEME        0x46	/* uriScheme */
#define TAG_CHARSET          0x47	/* charset */
#define TAG_NATULANG         0x48	/* naturalLanguage */
#define TAG_MIMETYPE         0x49	/* mimeMediaType */

struct ipp_hdr {
	int8_t  major_version;	/* always 1 */
	int8_t  minor_version;	/* always 1 */
	union {
		int16_t op;	/* operation ID */
		int16_t st;	/* status */
	} u;
	int32_t request_id;		/* request ID */
	char    attr_group[1];	/* start of optional attributes group */
	/* optional data follows */
};

#define operation u.op
#define status u.st

#endif /* _IPP_H */
