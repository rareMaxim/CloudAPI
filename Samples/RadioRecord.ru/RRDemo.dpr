program RRDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  RadioRecord,
  RadioRecord.Types,
  System.Classes,
  System.SysUtils;

procedure CreatePls(const ABitrate: Integer);
const
  M3U_ITEM = '#EXTINF:-1,%s'#13#10'%s';
var
  RR: TRadioRecord;
  Station: IrrStation;
  Plt: TStringList;
  LStream: string;
begin
  Write('Создаю плейлист с качеством потока: ' + ABitrate.ToString + '... ');
  RR := TRadioRecord.Create(nil);
  Plt := TStringList.Create;
  try
    Plt.Add('#EXTM3U');
    for Station in RR.GetStations do
    begin
      case ABitrate of
        32:
          LStream := Station.stream_32;
        64:
          LStream := Station.stream_64;
        128:
          LStream := Station.stream_128;
        320:
          LStream := Station.stream_320;
      else
        raise Exception.Create('Unknown bitrate');
      end;
      Plt.Add(Format(M3U_ITEM, [Station.title, LStream]));
    end;
    Plt.SaveToFile('radiorecord' + ABitrate.ToString + '.m3u');
    Writeln('Готово');
  finally
    RR.Free;
    Plt.Free;
  end;
end;

procedure Main;
begin
  CreatePls(32);
  CreatePls(64);
  CreatePls(128);
  CreatePls(320);
  Writeln('Для закрытия нажмите Enter');
end;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    Main;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.

