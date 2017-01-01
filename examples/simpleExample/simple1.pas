{simple example like on page http://proj4.org/development/api.html
showing basic usage}
program simple1;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  proj4api,
  pj_types;

  procedure method1Transform;
  var
    projlib: TProj4;
    pj_merc, pj_latlong: projPJ;
    x, y, z: double;
  begin
    projlib := TProj4.Create('../../Proj4 binaries/Linux/amd64/libproj.so.12.0.0');
    WriteLn('proj.4 version: ' + projlib.libraryVersion);
    pj_merc := nil;
    pj_latlong := nil;
    pj_merc := projlib.init_plus('+proj=merc +ellps=clrk66 +lat_ts=33');
    pj_latlong := projlib.init_plus('+proj=latlong +ellps=clrk66');
    if (pj_merc = nil) or (pj_latlong = nil) then
    begin
      WriteLn(projlib.GetLastError);
      halt(-1);
    end;
    x := -16 * DEG_TO_RAD;
    y := 20.25 * DEG_TO_RAD;
    z := 0;
    projlib.transform(pj_latlong, pj_merc, 1, 1, x, y, z);
    WriteLn(Format('%.2f %.2f', [x, y]));
    projlib.FreePJ(pj_merc);
    projlib.FreePJ(pj_latlong);
    projlib.Free;
  end;

  procedure method2Transform;
  var
    projlib: TProj4;
    x, y: double;
    projin, projout: string;
  begin
    x := 470267.6132;
    y := 4486639.8266;
    projin := '+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +datum=GGRS87 +units=m +no_defs';
    projout := '+proj=longlat +datum=WGS84 +no_defs';
    projlib := TProj4.Create('../../Proj4 binaries/Linux/amd64/libproj.so.12.0.0');
    projlib.ConvertLocalToTarget(projin, projout, x, y);
    WriteLn(Format('%.2f %.2f', [x, y]));
    projlib.Free;
  end;

begin
  method1Transform;
  method2Transform;
end.
