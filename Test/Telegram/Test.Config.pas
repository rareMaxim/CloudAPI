unit Test.Config;

interface

type
  TTestConfig = class
  private
    FToken: string;
    FUserId: Int64;
    FChannel: string;
  public
    property Token: string read Ftoken write Ftoken;
    property UserId: Int64 read FUserId write FUserId;
    property Channel: string read FChannel write FChannel;
  end;

  TTestConst = class
  public
    type
      Audio = class
      end;

      Video = class
        class function VideoDir: string;
        class function MoonLanding: string;
        class function GoldenRatio:string;
      end;
  public
    class function TestPath: string;
    class function Files: string;
  end;

function TestConfig: TTestConfig;

implementation

uses
  CloudAPI.Utils.Json,
  System.SysUtils;

resourcestring
  StrConfigtestjson = 'config.test.json';

var
  __TestConfig: TTestConfig;

function TestConfig: TTestConfig;
begin
  Result := __TestConfig;
end;

{ TTestConst }

class function TTestConst.Files: string;
begin
  Result := TestPath + 'Files\';
end;

class function TTestConst.TestPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

{ TTestConst.Video }

class function TTestConst.Video.GoldenRatio: string;
begin
   Result := VideoDir + 'golden-ratio-240px.mp4';
end;

class function TTestConst.Video.MoonLanding: string;
begin
  Result := VideoDir + 'moon-landing.mp4';
end;

class function TTestConst.Video.VideoDir: string;
begin
  Result := TTestConst.Files + 'Video\';
end;

initialization
  __TestConfig := TJsonUtils.FileToObject<TTestConfig>(StrConfigtestjson);

finalization
  __TestConfig.Free;

end.

