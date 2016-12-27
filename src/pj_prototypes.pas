{Pascal wrappper for proj.4 library}
{C prototypes}
{* Author:   Giannis Giannoulas, <gg@iqsoft.gr>
*
******************************************************************************
* Copyright (c) 2016,Giannis Giannoulas
*
* Permission is hereby granted, free of charge, to any person obtaining a
* copy of this software and associated documentation files (the "Software"),
* to deal in the Software without restriction, including without limitation
* the rights to use, copy, modify, merge, publish, distribute, sublicense,
* and/or sell copies of the Software, and to permit persons to whom the
* Software is furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included
* in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
* OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO COORD SHALL
* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
* DEALINGS IN THE SOFTWARE.
*****************************************************************************}
unit pj_prototypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pj_types, ctypes;

var
  (* procedure prototypes *)
  pj_fwd: function(val: projLP; proj: projPJ): projXY; cdecl;
  pj_inv: function(val: projXY; proj: projPJ): projLP; cdecl;
  pj_fwd3d: function(val: projLPZ; proj: projPJ): projXYZ; cdecl;
  pj_inv3d: function(val: projXYZ; proj: projPJ): projLPZ; cdecl;
  pj_transform: function(src, dst: projPJ; point_count: clong; point_offset: cint; x, y, z: pcdouble): cint; cdecl;
  pj_datum_transform: function(src, dst: projPJ; point_count: clong; point_offset: cint; x, y, z: pcdouble): cint; cdecl;
  pj_geocentric_to_geodetic: function(a, es: cdouble; point_count: clong; point_offset: cint; x, y, z: pcdouble): cint; cdecl;
  pj_geodetic_to_geocentric: function(a, es: cdouble; point_count: clong; point_offset: cint; x, y, z: pcdouble): cint; cdecl;
  pj_compare_datums: function(srcdefn: projPJ; dstdefn: projPJ): cint; cdecl;
  pj_apply_gridshift: function(ctx: projCTX; c: PChar; i: cint; point_count: clong; point_offset: cint; x, y, z: pcdouble): cint; cdecl;

  pj_deallocate_grids: procedure; cdecl;
  pj_clear_initcache: procedure; cdecl;
  pj_is_latlong: function(proj: projPJ): cint; cdecl;
  pj_is_geocent: function(proj: projPJ): cint; cdecl;
  pj_get_spheroid_defn: procedure(defn: projPJ; major_axis, eccentricity_squared: cdouble) cdecl;
  pj_pr_list: procedure(proj: projPJ); cdecl;
  pj_free: procedure(proj: projPJ); cdecl;
  pj_set_finder: procedure(finder: TprojFinder); cdecl;
  pj_set_searchpath: procedure(Count: cint; path: ppchar); cdecl;
  pj_init: function(argc: cint; argv: ppchar): projPJ; cdecl;
  pj_init_plus: function(args: PChar): projPJ; cdecl;
  pj_init_ctx: function(ctx: projCtx; argc: cint; argv: ppchar): projPJ; cdecl;
  pj_init_plus_ctx: function(ctx: projCtx; args: PChar): projPJ; cdecl;
  pj_get_def: function(proj: projPJ; i: cint): PChar; cdecl;
  pj_latlong_from_proj: function(proj: projPJ): projPJ; cdecl;
  pj_malloc: function(size: csize_t): pointer; cdecl;
  pj_dalloc: procedure(ptr: pointer); cdecl;
  pj_calloc: function(n: csize_t; size: csize_t): pointer cdecl;
  pj_dealloc: function(ptr: pointer): pointer cdecl;
  pj_strerrno: function(err: cint): PChar; cdecl;
  pj_get_errno_ref: function: pcint; cdecl;
  pj_get_release: function: PChar; cdecl;
  pj_errno: function: cint; cdecl;
  pj_acquire_lock: procedure; cdecl;
  pj_release_lock: procedure; cdecl;
  pj_cleanup_lock: procedure; cdecl;
  pj_get_default_ctx: function: projCTX; cdecl;
  pj_get_ctx: function(proj: projPJ): projCtx; cdecl;
  pj_set_ctx: procedure(proj: projPJ; ctx: projCtx); cdecl;
  pj_ctx_alloc: function: projCtx; cdecl;
  pj_ctx_free: procedure(ctx: projCtx); cdecl;
  pj_ctx_get_errno: function(ctx: projCtx): cint; cdecl;
  pj_ctx_set_errno: procedure(ctx: projCtx; no: cint); cdecl;
  pj_ctx_set_debug: procedure(ctx: projCtx; no: cint); cdecl;
  pj_ctx_set_logger: procedure(ctx: projCtx; logger: Tprojlogger);
  pj_ctx_set_app_data: procedure(ctx: projCtx; ptr: pointer); cdecl;
  pj_ctx_get_app_data: function(ctx: projCtx): pointer; cdecl;
  pj_ctx_set_fileapi: procedure(ctx: projCtx; api: PprojFileAPI); cdecl;
  pj_ctx_get_fileapi: function(ctx: projCtx): PprojFileAPI; cdecl;
  pj_log: procedure(ctx: projCtx; level: integer; fmt: PChar; Args: Pointer); cdecl;
  pj_stderr_logger: procedure(a: Pointer; i: integer; c: PChar); cdecl;
  pj_get_default_fileapi: function: PprojFileAPI; cdecl;
  pj_ctx_fopen: function(ctx: projCtx; filename, access: PChar): PAFile; cdecl;
  pj_ctx_fread: function(ctx: projCtx; buffer: Pointer; size, nmemb: csize_t; file_: PAFile): csize_t; cdecl;
  pj_ctx_fseek: function(ctx: projCtx; file_: PAFile; offset: clong; whence: integer): integer; cdecl;
  pj_ctx_ftell: function(ctx: projCtx; file_: PAFile): clong; cdecl;
  pj_ctx_fclose: procedure(ctx: projCtx; file_: PAFile); cdecl;
  pj_ctx_fgets: function(ctx: projCtx; line: PChar; size: integer; file_: PAFile): PChar; cdecl;
  pj_open_lib: function(ctx: projCtx; Name, mode: PChar): PAFile; cdecl;

{
  pj_create: function(definition: PChar): PJ; cdecl;
  pj_create_argv: function(argc: cint; argv: PPChar): PJ; cdecl;
  pj_freePJ: procedure(P: PJ); cdecl;
  pj_error: function(P: PJ): cint; cdecl;
  pj_trans: function(P: PJ; direction: pj_direction; obs: PJ_OBS): PJ_OBS; cdecl;

  { Measure internal consistency - in forward or inverse direction }
  pj_roundtrip: function(P: PJ; direction: pj_direction; n: cint; obs: PJ_OBS): cdouble; cdecl;

  { Geodesic distance between two points with angular 2D coordinates }
  pj_lp_dist: function(P: PJ; a, b: LP): cdouble; cdecl;

  { Euclidean distance between two points with linear 2D coordinates }
  pj_xy_dist: function(a, b: XY): cdouble; cdecl;

  { Euclidean distance between two points with linear 3D coordinates }
  pj_xyz_dist: function(a, b: XYZ): cdouble; cdecl;

  pj_err_level: function(P: PJ; err_level: cint): cint; cdecl;
     }
{ Set logging level 0-3. Higher number means more debug info. 0 turns it off }
//enum pj_log_level pj_log_level (PJ *P, enum pj_log_level log_level);

//void pj_log_error (PJ *P, const char *fmt, ...);
//void pj_log_debug (PJ *P, const char *fmt, ...);
//void pj_log_trace (PJ *P, const char *fmt, ...);
//void pj_log_func (PJ *P, void *app_data, void (*log)(void *, int, const char *));


{ Lower level functionality for handling thread contexts }
//int  pj_context_renew (PJ *P);
//void pj_context_inherit (PJ *mother, PJ *daughter);
//void pj_context_free    (const PJ *P);

{ Lowest level: Minimum support for fileapi }
//void pj_fileapi_set (PJ *P, void *fileapi);

implementation

end.

