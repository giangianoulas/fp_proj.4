# fp_proj.4
Free pascal bindings for PROJ.4 - Cartographic Projections Library [from here](http://proj4.org/)


### Usage
[http://proj4.org/development/api.html](http://proj4.org/development/api.html)
```pascal
var
    projlib: TProj4;
    pj_merc, pj_latlong: projPJ;
    x, y, z: double;
  begin
    {absolute path to dll}
    projlib := TProj4.Create('..\Proj4 binaries\Windows\mingw\x64\libproj-12.dll');
    WriteLn('proj.4 version: ' + projlib.libraryVersion);
    {or simply...}
    //projlib := TProj4.Create'
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
    projlib.transform(pj_latlong, pj_merc, 1, 1, x, y);
    WriteLn(Format('%.2f %.2f', [x, y]));
    projlib.FreePJ(pj_merc);
    projlib.FreePJ(pj_latlong);
    projlib.Free;
  end;
```

### [Official documentation](http://proj4.org/development/api.html)


### Directories
1. src -> pascal sources
2. Proj4 binaries -> prebuild binaries for convenience, build with default options
3. examples -> usage examples
