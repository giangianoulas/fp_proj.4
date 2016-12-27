{Pascal wrappper for proj.4 library}
{This License applies only to this wrapper and not the proj.4 itself}
{For the latest proj.4 visit https://github.com/OSGeo/proj.4}
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
unit proj4api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, dynlibs, pj_prototypes, pj_types;

const
  PJ_VERSION = 493;

type
  { TProj4 }
  TProj4 = class(TObject)
  private
    { private declarations }
    LibHandle: TLibHandle;
    LibFilename: string;
    libVersion: string;
  public
    { public declarations }
    constructor Create; overload;
    constructor Create(libpath: string); overload;
    destructor Destroy; override;
    function LoadProjLibrary: boolean;
    procedure ReleaseProjLibrary;
    procedure ConvertLocalToTarget(inputProj, outProj: string; out x, y: double);
    property libraryVersion: string read libVersion;
    property libraryFilename: string read LibFilename write LibFilename;
    function fwd(val: projLP; proj: projPJ): projXY;
    function inv(val: projXY; proj: projPJ): projLP;
    function fwd3d(val: projLPZ; proj: projPJ): projXYZ;
    function inv3d(val: projXYZ; proj: projPJ): projLPZ;
    function transform(src, dst: projPJ; point_count: longint; point_offset: integer; out x, y, z: double): integer;
    function datum_transform(src, dst: projPJ; point_count: longint; point_offset: integer; out x, y, z: double): integer;
    function geocentric_to_geodetic(a, es: double; point_count: longint; point_offset: integer; out x, y, z: double): integer;
    function geodetic_to_geocentric(a, es: double; point_count: longint; point_offset: integer; out x, y, z: double): integer;
    function compare_datums(srcdefn: projPJ; dstdefn: projPJ): integer;
    function apply_gridshift(ctx: projCTX; c: string; i: integer; point_count: longint; point_offset: integer; out x, y, z: double): integer;

    procedure deallocate_grids;
    procedure clear_initcache;
    function is_latlong(proj: projPJ): integer;
    function is_geocent(proj: projPJ): integer;
    procedure get_spheroid_defn(defn: projPJ; major_axis, eccentricity_squared: double);
    procedure pr_list(proj: projPJ);
    procedure FreePJ(proj: projPJ);
    procedure set_finder(finder: TprojFinder);
    procedure set_searchpath(Count: integer; path: string);
    function init(argc: integer; argv: string): projPJ;
    function init_plus(args: string): projPJ;
    function init_ctx(ctx: projCtx; argc: integer; argv: string): projPJ;
    function init_plus_ctx(ctx: projCtx; args: string): projPJ;
    function get_def(proj: projPJ; i: integer): string;
    function latlong_from_proj(proj: projPJ): projPJ;
    function malloc(size: csize_t): pointer;
    procedure dalloc(ptr: pointer);
    function calloc(n: csize_t; size: csize_t): pointer;
    function dealloc(ptr: pointer): pointer;
    function strerrno(err: integer): string;
    function get_errno_ref: integer;
    function get_release: string;
    procedure acquire_lock;
    procedure release_lock;
    procedure cleanup_lock;
    function get_default_ctx: projCTX;
    function get_ctx(proj: projPJ): projCtx;
    procedure set_ctx(proj: projPJ; ctx: projCtx);
    function ctx_alloc: projCtx;
    procedure ctx_free(ctx: projCtx);
    function ctx_get_errno(ctx: projCtx): integer;
    procedure ctx_set_errno(ctx: projCtx; no: integer);
    procedure ctx_set_debug(ctx: projCtx; no: integer);
    procedure ctx_set_logger(ctx: projCtx; logger: Tprojlogger);
    procedure ctx_set_app_data(ctx: projCtx; ptr: pointer);
    function ctx_get_app_data(ctx: projCtx): pointer;
    procedure ctx_set_fileapi(ctx: projCtx; api: PprojFileAPI);
    function ctx_get_fileapi(ctx: projCtx): PprojFileAPI;
    procedure log(ctx: projCtx; level: integer; fmt: string; Args: array of const);
    procedure stderr_logger(a: Pointer; i: integer; c: string);
    function get_default_fileapi: PprojFileAPI;
    function ctx_fopen(ctx: projCtx; filename, access: string): PAFile;
    function ctx_fread(ctx: projCtx; buffer: Pointer; size, nmemb: csize_t; file_: PAFile): csize_t;
    function ctx_fseek(ctx: projCtx; file_: PAFile; offset: longint; whence: integer): integer;
    function ctx_ftell(ctx: projCtx; file_: PAFile): longint;
    procedure ctx_fclose(ctx: projCtx; file_: PAFile);
    function ctx_fgets(ctx: projCtx; line: string; size: integer; file_: PAFile): string;
    function open_lib(ctx: projCtx; Name, mode: string): PAFile;

    //   pj_errno: function: cint; cdecl;
  end;

implementation

{ TProj4 }

function TProj4.LoadProjLibrary: boolean;
begin
  Result := False;
  LibHandle := LoadLibrary(PChar(LibFilename));
  if LibHandle = 0 then
    RaiseLastOsError;
  Pointer(pj_fwd) := GetProcAddress(LibHandle, 'pj_fwd');
  Pointer(pj_inv) := GetProcAddress(LibHandle, 'pj_inv');
  Pointer(pj_fwd3d) := GetProcAddress(LibHandle, 'pj_fwd3d');
  Pointer(pj_inv3d) := GetProcAddress(LibHandle, 'pj_fwd3d');
  Pointer(pj_transform) := GetProcAddress(LibHandle, 'pj_transform');
  Pointer(pj_datum_transform) := GetProcAddress(LibHandle, 'pj_datum_transform');
  Pointer(pj_geocentric_to_geodetic) := GetProcAddress(LibHandle, 'pj_geocentric_to_geodetic');
  Pointer(pj_compare_datums) := GetProcAddress(LibHandle, 'pj_compare_datums');
  Pointer(pj_apply_gridshift) := GetProcAddress(LibHandle, 'pj_apply_gridshift');
  Pointer(pj_deallocate_grids) := GetProcAddress(LibHandle, 'pj_deallocate_grids');
  Pointer(pj_clear_initcache) := GetProcAddress(LibHandle, 'pj_clear_initcache');
  Pointer(pj_is_latlong) := GetProcAddress(LibHandle, 'pj_is_latlong');
  Pointer(pj_is_geocent) := GetProcAddress(LibHandle, 'pj_is_geocent');
  Pointer(pj_pr_list) := GetProcAddress(LibHandle, 'pj_pr_list');
  Pointer(pj_free) := GetProcAddress(LibHandle, 'pj_free');
  Pointer(pj_set_finder) := GetProcAddress(LibHandle, 'pj_set_finder');
  Pointer(pj_set_searchpath) := GetProcAddress(LibHandle, 'pj_set_searchpath');
  Pointer(pj_init) := GetProcAddress(LibHandle, 'pj_init');
  Pointer(pj_init_plus) := GetProcAddress(LibHandle, 'pj_init_plus');
  Pointer(pj_init_ctx) := GetProcAddress(LibHandle, 'pj_init_ctx');
  Pointer(pj_init_plus_ctx) := GetProcAddress(LibHandle, 'pj_init_plus_ctx');
  Pointer(pj_get_def) := GetProcAddress(LibHandle, 'pj_get_def');
  Pointer(pj_latlong_from_proj) := GetProcAddress(LibHandle, 'pj_latlong_from_proj');
  Pointer(pj_malloc) := GetProcAddress(LibHandle, 'pj_malloc');
  Pointer(pj_dalloc) := GetProcAddress(LibHandle, 'pj_dalloc');
  Pointer(pj_calloc) := GetProcAddress(LibHandle, 'pj_calloc');
  Pointer(pj_dealloc) := GetProcAddress(LibHandle, 'pj_dealloc');
  Pointer(pj_strerrno) := GetProcAddress(LibHandle, 'pj_strerrno');
  Pointer(pj_get_release) := GetProcAddress(LibHandle, 'pj_get_release');
  Pointer(pj_acquire_lock) := GetProcAddress(LibHandle, 'pj_acquire_lock');
  Pointer(pj_release_lock) := GetProcAddress(LibHandle, 'pj_release_lock');
  Pointer(pj_cleanup_lock) := GetProcAddress(LibHandle, 'pj_cleanup_lock');
  Pointer(pj_get_default_ctx) := GetProcAddress(LibHandle, 'pj_get_default_ctx');
  Pointer(pj_get_ctx) := GetProcAddress(LibHandle, 'pj_get_ctx');
  Pointer(pj_set_ctx) := GetProcAddress(LibHandle, 'pj_set_ctx');
  Pointer(pj_ctx_alloc) := GetProcAddress(LibHandle, 'pj_ctx_alloc');
  Pointer(pj_ctx_free) := GetProcAddress(LibHandle, 'pj_ctx_free');
  Pointer(pj_ctx_get_errno) := GetProcAddress(LibHandle, 'pj_ctx_get_errno');
  Pointer(pj_ctx_set_errno) := GetProcAddress(LibHandle, 'pj_ctx_set_errno');
  Pointer(pj_ctx_set_debug) := GetProcAddress(LibHandle, 'pj_ctx_set_debug');
  Pointer(pj_ctx_set_app_data) := GetProcAddress(LibHandle, 'pj_ctx_set_app_data');
  Pointer(pj_ctx_get_app_data) := GetProcAddress(LibHandle, 'pj_ctx_get_app_data');
  Pointer(pj_get_spheroid_defn) := GetProcAddress(LibHandle, 'pj_get_spheroid_defn');
  Pointer(pj_ctx_set_logger) := GetProcAddress(LibHandle, 'pj_ctx_set_logger');
  Pointer(pj_ctx_set_fileapi) := GetProcAddress(LibHandle, 'pj_ctx_set_fileapi');
  Pointer(pj_ctx_get_fileapi) := GetProcAddress(LibHandle, 'pj_ctx_get_fileapi');
  Pointer(pj_log) := GetProcAddress(LibHandle, 'pj_log');
  Pointer(pj_stderr_logger) := GetProcAddress(LibHandle, 'pj_stderr_logger');
  Pointer(pj_ctx_fopen) := GetProcAddress(LibHandle, 'pj_ctx_fopen');
  Pointer(pj_ctx_fread) := GetProcAddress(LibHandle, 'pj_ctx_fread');
  Pointer(pj_ctx_fseek) := GetProcAddress(LibHandle, 'pj_ctx_fseek');
  Pointer(pj_ctx_ftell) := GetProcAddress(LibHandle, 'pj_ctx_ftell');
  Pointer(pj_ctx_fclose) := GetProcAddress(LibHandle, 'pj_ctx_fclose');
  Pointer(pj_ctx_fgets) := GetProcAddress(LibHandle, 'pj_ctx_fgets');
  Pointer(pj_open_lib) := GetProcAddress(LibHandle, 'pj_open_lib');

  {
  Pointer(pj_create) := GetProcAddress(LibHandle, 'pj_create');
  Pointer(pj_create_argv) := GetProcAddress(LibHandle, 'pj_create_argv');
  Pointer(pj_freePJ) := GetProcAddress(LibHandle, 'pj_freePJ');
  Pointer(pj_error) := GetProcAddress(LibHandle, 'pj_error');
  Pointer(pj_trans) := GetProcAddress(LibHandle, 'pj_trans');
  Pointer(pj_roundtrip) := GetProcAddress(LibHandle, 'pj_roundtrip');
  Pointer(pj_lp_dist) := GetProcAddress(LibHandle, 'pj_lp_dist');
  Pointer(pj_xy_dist) := GetProcAddress(LibHandle, 'pj_xy_dist');
  Pointer(pj_xyz_dist) := GetProcAddress(LibHandle, 'pj_xyz_dist');
  Pointer(pj_err_level) := GetProcAddress(LibHandle, 'pj_err_level');
  }

  if not (Assigned(pj_fwd) and Assigned(pj_inv) and Assigned(pj_transform) and Assigned(pj_datum_transform) and
    Assigned(pj_geocentric_to_geodetic) and Assigned(pj_compare_datums) and Assigned(pj_apply_gridshift) and
    Assigned(pj_deallocate_grids) and Assigned(pj_is_latlong) and Assigned(pj_is_geocent) and Assigned(pj_pr_list) and
    Assigned(pj_set_finder) and Assigned(pj_set_searchpath) and Assigned(pj_init) and Assigned(pj_init_plus) and
    Assigned(pj_get_def) and Assigned(pj_latlong_from_proj) and Assigned(pj_malloc) and Assigned(pj_dalloc) and
    Assigned(pj_strerrno) and Assigned(pj_get_release) and Assigned(pj_fwd3d) and Assigned(pj_inv3d) and
    Assigned(pj_clear_initcache) and Assigned(pj_get_spheroid_defn) and Assigned(pj_init_ctx) and Assigned(pj_init_plus_ctx) and
    Assigned(pj_calloc) and Assigned(pj_dealloc) and Assigned(pj_acquire_lock) and Assigned(pj_release_lock) and
    Assigned(pj_cleanup_lock) and Assigned(pj_get_default_ctx) and Assigned(pj_get_ctx) and Assigned(pj_set_ctx) and
    Assigned(pj_ctx_alloc) and Assigned(pj_ctx_free) and Assigned(pj_ctx_get_errno) and Assigned(pj_ctx_set_errno) and
    Assigned(pj_ctx_set_debug) and Assigned(pj_ctx_set_app_data) and Assigned(pj_ctx_get_app_data) and
    Assigned(pj_ctx_set_logger) and Assigned(pj_ctx_set_fileapi) and Assigned(pj_ctx_get_fileapi) and Assigned(pj_log) and
    Assigned(pj_stderr_logger) and Assigned(pj_ctx_fopen) and Assigned(pj_ctx_fread) and Assigned(pj_ctx_fseek) and
    Assigned(pj_ctx_ftell) and Assigned(pj_ctx_fclose) and Assigned(pj_ctx_fgets) and Assigned(pj_open_lib){ and
    Assigned(pj_create) and
    Assigned(pj_create_argv) and
    Assigned(pj_freePJ) and
    Assigned(pj_error) and
    Assigned(pj_trans) and
    Assigned(pj_roundtrip) and
    Assigned(pj_lp_dist) and
    Assigned(pj_xy_dist) and
    Assigned(pj_xyz_dist) and
    Assigned(pj_err_level)
}
) then
    raise Exception.Create('Unsupported proj4 library version');
  Result := True;
end;

procedure TProj4.ReleaseProjLibrary;
begin
  if LibHandle <> 0 then
  begin
    FreeLibrary(LibHandle);
    LibHandle := 0;
  end;
end;

constructor TProj4.Create;
var
  loadstat: boolean;
begin
  inherited Create;
  {$IF Defined(WINDOWS)}
  LibFilename := 'proj.dll';
  {$ELSEIF Defined(UNIX)}
  LibFilename := 'proj4/libproj.so.12.0.0';
  {$ENDIF}
  LibHandle := 0;
  loadstat := LoadProjLibrary;
  if loadstat then
    libVersion := StrPas(pj_get_release());
end;

constructor TProj4.Create(libpath: string);
var
  loadstat: boolean;
begin
  inherited Create;
  LibFilename := libpath;
  LibHandle := 0;
  loadstat := LoadProjLibrary;
  if loadstat then
    libVersion := StrPas(pj_get_release());
end;

destructor TProj4.Destroy;
begin
  ReleaseProjLibrary;
  inherited Destroy;
end;

procedure TProj4.ConvertLocalToTarget(inputProj, outProj: string; out x, y: double);
var
  inp, outp: projPJ;
  px, py: pcdouble;
begin
  inp := nil;
  outp := nil;
  inp := pj_init_plus(PChar(inputProj));
  outp := pj_init_plus(PChar(outProj));
  if inp = nil then
  begin
    x := -1;
    y := -1;
    Exit;
  end;
  if outp = nil then
  begin
    x := -1;
    y := -1;
    Exit;
  end;
  px := @x;
  py := @y;
  pj_transform(inp, outp, 1, 1, px, py, nil);
  x := x * RAD_TO_DEG;
  y := y * RAD_TO_DEG;
  pj_free(inp);
  pj_free(outp);
end;

function TProj4.fwd(val: projLP; proj: projPJ): projXY;
begin
  Result := pj_fwd(val, proj);
end;

function TProj4.inv(val: projXY; proj: projPJ): projLP;
begin
  Result := pj_inv(val, proj);
end;

function TProj4.fwd3d(val: projLPZ; proj: projPJ): projXYZ;
begin
  Result := pj_fwd3d(val, proj);
end;

function TProj4.inv3d(val: projXYZ; proj: projPJ): projLPZ;
begin
  Result := pj_inv3d(val, proj);
end;

function TProj4.transform(src, dst: projPJ; point_count: longint; point_offset: integer; out x, y, z: double): integer;
begin
  Result := pj_transform(src, dst, point_count, point_offset, @x, @y, @z);
end;

function TProj4.datum_transform(src, dst: projPJ; point_count: longint; point_offset: integer; out x, y, z: double): integer;
begin
  Result := pj_datum_transform(src, dst, point_count, point_offset, @x, @y, @z);
end;

function TProj4.geocentric_to_geodetic(a, es: double; point_count: longint; point_offset: integer; out x, y, z: double): integer;
begin
  Result := pj_geocentric_to_geodetic(a, es, point_count, point_offset, @x, @y, @z);
end;

function TProj4.geodetic_to_geocentric(a, es: double; point_count: longint; point_offset: integer; out x, y, z: double): integer;
begin
  Result := pj_geodetic_to_geocentric(a, es, point_count, point_offset, @x, @y, @z);
end;

function TProj4.compare_datums(srcdefn: projPJ; dstdefn: projPJ): integer;
begin
  Result := pj_compare_datums(srcdefn, dstdefn);
end;

function TProj4.apply_gridshift(ctx: projCTX; c: string; i: integer; point_count: longint; point_offset: integer; out x, y, z: double): integer;
begin
  Result := pj_apply_gridshift(ctx, PChar(c), i, point_count, point_offset, @x, @y, @z);
end;

procedure TProj4.deallocate_grids;
begin
  pj_deallocate_grids;
end;

procedure TProj4.clear_initcache;
begin
  pj_clear_initcache();
end;

function TProj4.is_latlong(proj: projPJ): integer;
begin
  Result := pj_is_latlong(proj);
end;

function TProj4.is_geocent(proj: projPJ): integer;
begin
  Result := pj_is_geocent(proj);
end;

procedure TProj4.get_spheroid_defn(defn: projPJ; major_axis, eccentricity_squared: double);
begin
  pj_get_spheroid_defn(defn, major_axis, eccentricity_squared);
end;

procedure TProj4.pr_list(proj: projPJ);
begin
  pj_pr_list(proj);
end;

procedure TProj4.FreePJ(proj: projPJ);
begin
  pj_Free(proj);
end;

procedure TProj4.set_finder(finder: TprojFinder);
begin
  pj_set_finder(finder);
end;

procedure TProj4.set_searchpath(Count: integer; path: string);
begin
  pj_set_searchpath(Count, StringToPPChar(path, 0));
end;

function TProj4.init(argc: integer; argv: string): projPJ;
begin
  Result := pj_init(argc, StringToPPChar(argv, 0));
end;

function TProj4.init_plus(args: string): projPJ;
begin
  Result := pj_init_plus(PChar(args));
end;

function TProj4.init_ctx(ctx: projCtx; argc: integer; argv: string): projPJ;
begin
  Result := pj_init_ctx(ctx, argc, StringToPPChar(argv, 0));
end;

function TProj4.init_plus_ctx(ctx: projCtx; args: string): projPJ;
begin
  Result := pj_init_plus_ctx(ctx, PChar(args));
end;

function TProj4.get_def(proj: projPJ; i: integer): string;
begin
  Result := StrPas(pj_get_def(proj, i));
end;

function TProj4.latlong_from_proj(proj: projPJ): projPJ;
begin
  Result := pj_latlong_from_proj(proj);
end;

function TProj4.malloc(size: csize_t): pointer;
begin
  Result := pj_malloc(size);
end;

procedure TProj4.dalloc(ptr: pointer);
begin
  pj_dalloc(ptr);
end;

function TProj4.calloc(n: csize_t; size: csize_t): pointer;
begin
  Result := pj_calloc(n, size);
end;

function TProj4.dealloc(ptr: pointer): pointer;
begin
  Result := pj_dealloc(ptr);
end;

function TProj4.strerrno(err: integer): string;
begin
  Result := StrPas(pj_strerrno(err));
end;

function TProj4.get_errno_ref: integer;
var
  p: ^integer;
begin
  p := @Result;
  p := pj_get_errno_ref();
end;

function TProj4.get_release: string;
begin
  Result := StrPas(pj_get_release());
end;

procedure TProj4.acquire_lock;
begin
  pj_acquire_lock;
end;

procedure TProj4.release_lock;
begin
  pj_release_lock;
end;

procedure TProj4.cleanup_lock;
begin
  pj_cleanup_lock;
end;

function TProj4.get_default_ctx: projCTX;
begin
  Result := pj_get_default_ctx;
end;

function TProj4.get_ctx(proj: projPJ): projCtx;
begin
  Result := pj_get_ctx(proj);
end;

procedure TProj4.set_ctx(proj: projPJ; ctx: projCtx);
begin
  pj_set_ctx(proj, ctx);
end;

function TProj4.ctx_alloc: projCtx;
begin
  Result := pj_ctx_alloc;
end;

procedure TProj4.ctx_free(ctx: projCtx);
begin
  pj_ctx_free(ctx);
end;

function TProj4.ctx_get_errno(ctx: projCtx): integer;
begin
  Result := pj_ctx_get_errno(ctx);
end;

procedure TProj4.ctx_set_errno(ctx: projCtx; no: integer);
begin
  pj_ctx_set_errno(ctx, no);
end;

procedure TProj4.ctx_set_debug(ctx: projCtx; no: integer);
begin
  pj_ctx_set_debug(ctx, no);
end;

procedure TProj4.ctx_set_logger(ctx: projCtx; logger: Tprojlogger);
begin
  pj_ctx_set_logger(ctx, logger);
end;

procedure TProj4.ctx_set_app_data(ctx: projCtx; ptr: pointer);
begin
  pj_ctx_set_app_data(ctx, ptr);
end;

function TProj4.ctx_get_app_data(ctx: projCtx): pointer;
begin
  Result := pj_ctx_get_app_data(ctx);
end;

procedure TProj4.ctx_set_fileapi(ctx: projCtx; api: PprojFileAPI);
begin
  pj_ctx_set_fileapi(ctx, api);
end;

function TProj4.ctx_get_fileapi(ctx: projCtx): PprojFileAPI;
begin
  Result := pj_ctx_get_fileapi(ctx);
end;

procedure TProj4.log(ctx: projCtx; level: integer; fmt: string; Args: array of const);
var
  ElsArray, El: PDWORD;
  I: integer;
  P: PDWORD;
begin
  ElsArray := nil;
  if High(Args) >= 0 then
    GetMem(ElsArray, (High(Args) + 1) * sizeof(Pointer));
  El := ElsArray;
  for I := 0 to High(Args) do
  begin
    P := @Args[I];
    P := Pointer(P^);
    El^ := DWORD(P);
    Inc(El);
  end;
  pj_log(ctx, level, PChar(fmt), Pointer(ElsArray));
  if ElsArray <> nil then
    FreeMem(ElsArray);
end;

procedure TProj4.stderr_logger(a: Pointer; i: integer; c: string);
begin
  pj_stderr_logger(a, i, PChar(c));
end;

function TProj4.get_default_fileapi: PprojFileAPI;
begin
  Result := pj_get_default_fileapi();
end;

function TProj4.ctx_fopen(ctx: projCtx; filename, access: string): PAFile;
begin
  Result := pj_ctx_fopen(ctx, PChar(filename), PChar(access));
end;

function TProj4.ctx_fread(ctx: projCtx; buffer: Pointer; size, nmemb: csize_t; file_: PAFile): csize_t;
begin
  Result := pj_ctx_fread(ctx, buffer, size, nmemb, file_);
end;

function TProj4.ctx_fseek(ctx: projCtx; file_: PAFile; offset: longint; whence: integer): integer;
begin
  Result := pj_ctx_fseek(ctx, file_, offset, whence);
end;

function TProj4.ctx_ftell(ctx: projCtx; file_: PAFile): longint;
begin
  Result := pj_ctx_ftell(ctx, file_);
end;

procedure TProj4.ctx_fclose(ctx: projCtx; file_: PAFile);
begin
  pj_ctx_fclose(ctx, file_);
end;

function TProj4.ctx_fgets(ctx: projCtx; line: string; size: integer; file_: PAFile): string;
begin
  Result := StrPas(pj_ctx_fgets(ctx, PChar(line), size, file_));
end;

function TProj4.open_lib(ctx: projCtx; Name, mode: string): PAFile;
begin
  Result := pj_open_lib(ctx, PChar(Name), PChar(mode));
end;

end.
