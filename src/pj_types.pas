{Pascal wrappper for proj.4 library}
{Types defined in proj_api.h}
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
unit pj_types;

{$mode objfpc}{$H+}

interface

uses
  ctypes;

const
  RAD_TO_DEG = 57.29577951308232;
  DEG_TO_RAD = 0.0174532925199432958;
  PJ_IO_UNITS_CLASSIC = 0;
  PJ_IO_UNITS_METERS = 1;
  PJ_IO_UNITS_RADIANS = 2;
  MAX_TAB_ID = 80;

type
  PJ_CSTR = PAnsiChar;
  PPJ_CSTR = ^PAnsiChar;

  {$PackRecords 1}
  projUV = record
    u, v: cdouble;
  end;

  projUVW = record
    u, v, w: cdouble;
  end;

  projPJ = ^TPJconsts;//pointer;

  projXY = projUV;
  projLP = projUV;
  projXYZ = projUVW;
  projLPZ = projUVW;
  projCTX = pointer;

  PAFile = ^integer;
  TprojFinder = function(s: pj_cstr): pj_cstr of object;
  Tprojlogger = procedure(ptr: pointer; n: cint; s: pj_cstr) of object;
  TProjFopen = function(ctx: projCTX; filename, access: pj_cstr): PAFile; cdecl;
  TProjFRead = function(buffer: Pointer; size, nmemb: csize_t; file_: PAFile): csize_t; cdecl;
  TProjFSeek = function(file_: PAFile; offset: clong; whence: integer): integer; cdecl;
  TProjFtell = function(file_: PAFile): clong;
  TProjFClose = procedure(file_: PAFile); cdecl;

  projFileAPI = record
    fopen: TProjFopen;
    fread: TProjFRead;
    fseek: TProjFSeek;
    ftell: TProjFtell;
    fclose: TProjFClose;
  end;
  PprojFileAPI = ^projFileAPI;

  PprojCtx_t = ^projCtx_t;

  projCtx_t = record
    last_errno: integer;
    debug_level: integer;
    logger: procedure(_para1: pointer; _para2: longint; _para3: pj_cstr); cdecl;
    app_data: Pointer;
    fileapi: PprojFileAPI;
  end;

  Pgeod_geodesic = ^Tgeod_geodesic;
  Tgeod_geodesic = record
    {undefined structure}
  end;

  Ppj_opaque = ^Tpj_opaque;
  Tpj_opaque = record
    {undefined structure}
  end;

  {proj.h}
  { Omega, Phi, Kappa: Rotations }
  PJ_OPK = record
    o, p, k: cDouble;
  end;
  {/* Easting, Northing, and some kind of height (orthometric or ellipsoidal) */}

  PJ_ENH = record
    e, n, h: cDouble;
  end;

  PJ_XYZT = record
    x, y, z, t: cDouble;
  end;

  PJ_ENHT = record
    e, n, h, t: cDouble;
  end;

  PJ_UVWT = record
    u, v, w, t: cDouble;
  end;

  PJ_LPZT = record
    lam, phi, z, t: cDouble;
  end;

  UV = record
    u, v: cDouble;
  end;

  XY = record
    x, y: cDouble;
  end;

  LP = record
    lam, phi: cDouble;
  end;

  XYZ = record
    x, y, z: cDouble;
  end;

  UVW = record
    u, v, w: cDouble;
  end;

  LPZ = record
    lam, phi, z: cDouble
  end;
  {/* Degrees, minutes, and seconds */}
  PJ_DMS = record
    d, m, s: cDouble;
  end;
  {/* Geoid undulation (N) and deflections of the vertical (eta, zeta) */}
  PJ_EZN = record
    e, z, N: cDouble;
  end;

  {/* Ellipsoidal parameters */}
  PJ_AF = record
    a, f: cDouble;
  end;

  pj_direction = (
    FWD = 1,   { Forward    }
    IDENT = 0,   { Do nothing }
    INV = -1    { Inverse    }
    );

  pj_log_level = (
    PJ_LOG_NONE = 0,
    PJ_LOG_ERROR = 1,
    PJ_LOG_DEBUG = 2,
    PJ_LOG_TRACE = 3,
    PJ_LOG_TELL = 4,
    PJ_LOG_DEBUG_MAJOR = 2, { for proj_api.h compatibility }
    PJ_LOG_DEBUG_MINOR = 3  { for proj_api.h compatibility }
    );

  PJ_COORD = record
    xyzt: PJ_XYZT;
    uvwt: PJ_UVWT;
    enht: PJ_ENHT;
    lpzt: PJ_LPZT;
    enh: PJ_ENH;
    v: array [0..3] of cDouble; { It's just a vector }
    xyz: XYZ;
    uvw: UVW;
    lpz: LPZ;
    xy: XY;
    uv: UV;
    lp: LP;
  end;

  PJ_TRIPLET = record
    opk: PJ_OPK;
    enh: PJ_ENH;
    ezn: PJ_EZN;
    dms: PJ_DMS;
    v: array [0..2] of cDouble; { It's just a vector }
    xyz: XYZ;
    lpz: LPZ;
    uvw: UVW;
    xy: XY;
    lp: LP;
    uv: UV;
    af: PJ_AF;
  end;

  PJ_PAIR = record
    xy: XY;
    lp: LP;
    uv: UV;
    af: PJ_AF;
    v: array [0..1] of cDouble; { It's just a vector }
  end;

  PJ_OBS = record
    coo: PJ_COORD;        { coordinate data }
    anc: PJ_TRIPLET;      { ancillary data }
    id: cint;              { integer ancillary data - e.g. observation number, EPSG code... }
    flags: cuint;  { additional data, intended for flags }
  end;

  PJ_FILE = ^cint;

  PJ = ^TPJconsts;//pointer;

  { derivatives of x for lambda-phi  }
  { derivatives of y for lambda-phi  }
  PDERIVS = ^TDERIVS;

  TDERIVS = record
    x_l: double;
    x_p: double;
    y_l: double;
    y_p: double;
  end;

  { parameter list  }
  { LEFT: Radians     RIGHT: Scaled meters  }
  { Meters   }
  { Radians  }
  Ppj_io_units = ^Tpj_io_units;
  Tpj_io_units = longint;

  { meridional, parallel scales  }
  { angular distortion, theta prime  }
  { convergence  }
  { areal scale factor  }
  { max-min scale error  }
  { info as to analytics, see following  }
  PFACTORS = ^TFACTORS;

  TFACTORS = record
    der: TDERIVS;
    h: double;
    k: double;
    omega: double;
    thetap: double;
    conv: double;
    s: double;
    a: double;
    b: double;
    code: longint;
  end;

  PPJ_REGION = ^TPJ_REGION;

  TPJ_REGION = record
    ll_long: double;
    ll_lat: double;
    ur_long: double;
    ur_lat: double;
  end;

  PFLP = ^TFLP;

  TFLP = record
    lam: single;
    phi: single;
  end;

  PILP = ^TILP;

  TILP = record
    lam: longint;
    phi: longint;
  end;

  { conversion matrix  }
  PCTABLE = ^TCTABLE;

  TCTABLE = record
    id: array[0..(MAX_TAB_ID) - 1] of char;
    ll: LP;
    del: LP;
    lim: TILP;
    cvs: PFLP;
  end;

  P_pj_gi = ^T_pj_gi;

  T_pj_gi = record
    gridname: pj_cstr;
    filename: pj_cstr;
    format: pj_cstr;
    grid_offset: longint;
    must_swap: longint;
    ct: PCTABLE;
    Next: P_pj_gi;
    child: P_pj_gi;
  end;
  TPJ_GRIDINFO = T_pj_gi;
  PPJ_GRIDINFO = ^TPJ_GRIDINFO;

  PPJ_GridCatalogEntry = ^TPJ_GridCatalogEntry;

  TPJ_GridCatalogEntry = record
    region: TPJ_Region;
    priority: longint;
    date: double;
    definition: pj_cstr;
    gridinfo: PPJ_GRIDINFO;
    available: longint;
  end;
  { maximum extent of catalog data  }

  P_PJ_GridCatalog = ^T_PJ_GridCatalog;

  T_PJ_GridCatalog = record
    catalog_name: pj_cstr;
    region: TPJ_Region;
    entry_count: longint;
    entries: PPJ_GridCatalogEntry;
    Next: P_PJ_GridCatalog;
  end;
  TPJ_GridCatalog = T_PJ_GridCatalog;
  PPJ_GridCatalog = ^TPJ_GridCatalog;

  paralist = ^ARG_list;

  ARG_list = record
    Next: paralist;
    used: char;
    param: char;
  end;


  PPJconsts = ^TPJconsts;
  PPJ = PPJconsts;

  TPJconsts = record
    ctx: PprojCtx_t;
    descr: pj_cstr;
    params: paralist;
    geod: Pgeod_geodesic;
    opaque: Ppj_opaque;
    fwd: function(_para1: LP; _para2: PPJ): XY; cdecl;
    inv: function(_para1: XY; _para2: PPJ): LP; cdecl;
    fwd3d: function(_para1: LPZ; _para2: PPJ): XYZ; cdecl;
    inv3d: function(_para1: XYZ; _para2: PPJ): LPZ; cdecl;
    fwdobs: function(_para1: PJ_OBS; _para2: PPJ): PJ_OBS; cdecl;
    invobs: function(_para1: PJ_OBS; _para2: PPJ): PJ_OBS; cdecl;
    spc: procedure(_para1: LP; _para2: PPJ; _para3: PFACTORS); cdecl;
    pfree: procedure(_para1: PPJ); cdecl;
    a: double;
    b: double;
    ra: double;
    rb: double;
    alpha: double;
    e: double;
    es: double;
    e2: double;
    e2s: double;
    e3: double;
    e3s: double;
    one_es: double;
    rone_es: double;
    f: double;
    f2: double;
    n: double;
    rf: double;
    rf2: double;
    rn: double;
    J: double;
    es_orig: double;
    a_orig: double;
    over: longint;
    geoc: longint;
    is_latlong: longint;
    is_geocent: longint;
    left: Tpj_io_units;
    right: Tpj_io_units;
    lam0: double;
    phi0: double;
    x0: double;
    y0: double;
    k0: double;
    to_meter: double;
    fr_meter: double;
    vto_meter: double;
    vfr_meter: double;
    datum_type: longint;
    datum_params: array[0..6] of double;
    gridlist: ^P_pj_gi;
    gridlist_count: longint;
    has_geoid_vgrids: longint;
    vgridlist_geoid: ^P_pj_gi;
    vgridlist_geoid_count: longint;
    from_greenwich: double;
    long_wrap_center: double;
    is_long_wrap_set: longint;
    axis: array[0..3] of char;
    catalog_name: pj_cstr;
    catalog: P_PJ_GridCatalog;
    datum_date: double;
    last_before_grid: P_pj_gi;
    last_before_region: TPJ_Region;
    last_before_date: double;
    last_after_grid: P_pj_gi;
    last_after_region: TPJ_Region;
    last_after_date: double;
  end;

  PPJ_LIST = ^PJ_LIST;
  PJ_LIST = record
    id: pj_cstr;
    proj:function(prj:PJ):PJ; cdecl;
    descr: pj_cstr;
  end;

  {$PackRecords default}
implementation

end.
