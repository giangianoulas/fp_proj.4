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
  Classes, SysUtils, ctypes;

const
  RAD_TO_DEG = 57.29577951308232;
  DEG_TO_RAD = 0.0174532925199432958;

type
  projUV = record
    u, v: cdouble;
  end;

  projUVW = record
    u, v, w: cdouble;
  end;

  projPJ = pointer;
  projXY = projUV;
  projLP = projUV;
  projXYZ = projUVW;
  projLPZ = projUVW;
  projCTX = pointer;

  PAFile = ^integer;
  TprojFinder = function(s: PChar): PChar of object;
  Tprojlogger = procedure(ptr: pointer; n: cint; s: PChar) of object;
  TProjFopen = function(ctx: projCTX; filename, access: PChar): PAFile of object;
  TProjFRead = function(buffer: Pointer; size, nmemb: csize_t; file_: PAFile): csize_t of object;
  TProjFSeek = function(file_: PAFile; offset: clong; whence: integer): integer of object;
  TProjFtell = function(file_: PAFile): clong of object;
  TProjFClose = procedure(file_: PAFile) of object;

  projFileAPI = record
    fopen: TProjFopen;
    fread: TProjFRead;
    fseek: TProjFSeek;
    ftell: TProjFtell;
    fclose: TProjFClose;
  end;
  PprojFileAPI = ^projFileAPI;
  {
  {proj.h}
  { Omega, Phi, Kappa: Rotations }
  PJ_OPK = record
    o, p, k: cdouble;
  end;
  {/* Easting, Northing, and some kind of height (orthometric or ellipsoidal) */}

  PJ_ENH = record
    e, n, h: cdouble;
  end;

  PJ_XYZT = record
    x, y, z, t: cdouble;
  end;

  PJ_ENHT = record
    e, n, h, t: cdouble;
  end;

  PJ_UVWT = record
    u, v, w, t: cdouble;
  end;

  PJ_LPZT = record
    lam, phi, z, t: cdouble;
  end;

  UV = record
    u, v: cdouble;
  end;

  XY = record
    x, y: cdouble;
  end;

  LP = record
    lam, phi: cdouble;
  end;

  XYZ = record
    x, y, z: cdouble;
  end;

  UVW = record
    u, v, w: cdouble;
  end;

  LPZ = record
    lam, phi, z: cdouble
  end;
  {/* Degrees, minutes, and seconds */}
  PJ_DMS = record
    d, m, s: cdouble;
  end;
  {/* Geoid undulation (N) and deflections of the vertical (eta, zeta) */}
  PJ_EZN = record
    e, z, N: cdouble;
  end;

  {/* Ellipsoidal parameters */}
  PJ_AF = record
    a, f: cdouble;
  end;

  pj_direction = (
    FWD = 1,   { Forward    }
    IDENT = 0,   { Do nothing }
    INV = -1    { Inverse    }
    );

  pj_log_level = (
    PJ_LOG_NONE  = 0,
    PJ_LOG_ERROR = 1,
    PJ_LOG_DEBUG = 2,
    PJ_LOG_TRACE = 3,
    PJ_LOG_TELL  = 4,
    PJ_LOG_DEBUG_MAJOR = 2, { for proj_api.h compatibility }
    PJ_LOG_DEBUG_MINOR = 3  { for proj_api.h compatibility }
  );

  PJ_COORD = record
    xyzt: PJ_XYZT;
    uvwt: PJ_UVWT;
    enht: PJ_ENHT;
    lpzt: PJ_LPZT;
    enh: PJ_ENH;
    v: array [0..3] of cdouble; { It's just a vector }
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
    v: array [0..2] of cdouble; { It's just a vector }
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
    v: array [0..1] of cdouble; { It's just a vector }
  end;

  PJ_OBS = record
    coo: PJ_COORD;        { coordinate data }
    anc: PJ_TRIPLET;      { ancillary data }
    id: cint;              { integer ancillary data - e.g. observation number, EPSG code... }
    flags: cuint;  { additional data, intended for flags }
  end;

  PJ_FILE = ^cint;

  PJ = pointer;

  }
implementation

end.



