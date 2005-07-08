/* $Id: gvrenderint.h,v 1.17 2004/12/11 19:26:07 ellson Exp $ $Revision: 1.17 $ */
/* vim:set shiftwidth=4 ts=8: */

/**********************************************************
*      This software is part of the graphviz package      *
*                http://www.graphviz.org/                 *
*                                                         *
*            Copyright (c) 1994-2004 AT&T Corp.           *
*                and is licensed under the                *
*            Common Public License, Version 1.0           *
*                      by AT&T Corp.                      *
*                                                         *
*        Information and Software Systems Research        *
*              AT&T Research, Florham Park NJ             *
**********************************************************/


/* Common header used by both clients and plugins */

#ifndef GVRENDERINT_H
#define GVRENDERINT_H

#ifdef __cplusplus
extern "C" {
#endif

#define GVRENDER_DOES_ARROWS (1<<0)
#define GVRENDER_DOES_LAYERS (1<<1)
#define GVRENDER_DOES_MULTIGRAPH_OUTPUT_FILES (1<<2)

    typedef struct gvrender_job_s gvrender_job_t;

    struct gvrender_job_s {
	gvrender_job_t *next;
	char *output_filename;
	char *output_langname;
	FILE *output_file;
	void *interim_output;
	int output_lang;
    };

    typedef struct gv_plugin_s gv_plugin_t;

    struct gv_plugin_s {
	gv_plugin_t *next;
	int handle;
	char *path;
	char *type;
	void *fn;
    };

    struct GVC_s {
	/* gvNEWcontext() */
	char *user;
	char **info;
	/* gvrender_config() */
	gvrender_job_t *jobs;	/* linked list of jobs */
	gvrender_job_t *job;	/* current job */
	/* renderer plugins */
	int next_renderer_handle;	/* unique integer handle assigned to renderers on install */
	gv_plugin_t *renderers;	/* alpha-sorted linked list of renderers */
	/* gvrender_begin_job() */
	gvrender_engine_t *render_engine;
#if ENABLE_CODEGENS
	codegen_t *codegen;
#endif
	char **lib;
	point pages;
	/* gvrender_begin_graph() */
	graph_t *g;
	box bb;
	point pb;
	point size;		/* viewport size (pixels) */
	double zoom;		/* viewport zoom factor */
	pointf focus;		/* viewport focus in graph units */
	boolean onetime;
	/* gvrender_begin_page() */
	point page;
	double scale;
	int rot;
	point offset;
	/* gvrender_begin_layer() */
	char *layerName;
	int layer;
	int nLayers;
	/* gvrender_begin_cluster() */
	graph_t *sg;
	/* gvrender_begin_node() */
	node_t *n;
	/* gvrender_begin_edge() */
	edge_t *e;
    };

#ifdef __cplusplus
}
#endif
#endif
