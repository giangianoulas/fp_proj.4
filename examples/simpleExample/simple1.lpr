{simple example like on page https://trac.osgeo.org/proj/wiki/ProjAPI
showing basic usage}
program simple1;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  proj4api,
  pj_types { you can add units after this };

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure method1Transform;
    procedure method2Transform;
    destructor Destroy; override;
  end;

  { TMyApplication }

  procedure TMyApplication.DoRun;
  begin
    method1Transform;
    method2Transform;
    Terminate;
  end;

  constructor TMyApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  procedure TMyApplication.method1Transform;
  var
    projlib: TProj4;
    pj_merc, pj_latlong: projPJ;
    x, y, z: double;
  begin
    projlib := TProj4.Create('../../Proj4 binaries/Linux/amd64/libproj.so.12.0.0');
    pj_merc := nil;
    pj_latlong := nil;
    pj_merc := projlib.init_plus('+proj=merc +ellps=clrk66 +lat_ts=33');
    pj_latlong := projlib.init_plus('+proj=latlong +ellps=clrk66');
    if pj_merc = nil then
      Terminate;
    if pj_latlong = nil then
      Terminate;
    x := -16 * DEG_TO_RAD;
    y := 20.25 * DEG_TO_RAD;
    z := 0;
    projlib.transform(pj_latlong, pj_merc, 1, 1, x, y, z);
    WriteLn(Format('%.2f %.2f', [x, y]));
    projlib.FreePJ(pj_merc);
    projlib.FreePJ(pj_latlong);
    projlib.Free;
  end;

  procedure TMyApplication.method2Transform;
  var
    projlib: TProj4;
    x, y: double;
    projin, projout:string;
  begin
    x := 470267.6132;
    y := 4486639.8266;
    projin:= '+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +datum=GGRS87 +units=m +no_defs';
    projout:= '+proj=longlat +datum=WGS84 +no_defs';
    projlib := TProj4.Create('../../Proj4 binaries/Linux/amd64/libproj.so.12.0.0');
    projlib.ConvertLocalToTarget(projin, projout, x, y);
    WriteLn(Format('%.2f %.2f', [x, y]));
    projlib.Free;
  end;

  destructor TMyApplication.Destroy;
  begin
    inherited Destroy;
  end;

var
  Application: TMyApplication;
begin
  Application := TMyApplication.Create(nil);
  Application.Title := 'basicExample';
  Application.Run;
  Application.Free;
end.
