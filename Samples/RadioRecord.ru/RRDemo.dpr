program RRDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  RadioRecord,
  RadioRecord.Types,
  System.Classes,
  System.SysUtils;

procedure Test;
const
  M3U_ITEM = '#EXTINF:-1,%s'#13#10'%s';
var
  RR: TRadioRecord;
  Station: IrrStation;
  Plt: TStringList;
begin
  RR := TRadioRecord.Create(nil);
  Plt := TStringList.Create;
  try
    Plt.Add('#EXTM3U');
    for Station in RR.GetStations do
    begin
      Plt.Add(Format(M3U_ITEM, [Station.title, Station.stream_320]));
      Writeln(Station.title + ' - ' + Station.stream_320);
    end;
    Plt.SaveToFile('radiorecord.m3u');
  finally
    RR.Free;
    Plt.Free;
  end;

end;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    Test;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.

