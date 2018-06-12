program Demo;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Generics.Collections,
  VirusTotal.Types in '..\..\Source\virustotal.com\VirusTotal.Types.pas',
  VirusTotal in '..\..\Source\virustotal.com\VirusTotal.pas';

const
  API_KEY = 'e2fd0cd961bdeaf2d054871299a6c2f056d7a5dbda813b93000a81a64087b341';

procedure TestFileReport;
var
  VT: TVirusTotal;
  LRest: IvtReport;
  LScan: TPair<string, IvtScan>;
begin
  VT := TVirusTotal.Create(nil);
  VT.ApiKey := API_KEY;
  try
    LRest := VT.FileReport('58e7605a13b67233fe687953ceca1364caf07f97e8faddb8d25eb43c9e6f290a');
    Writeln(LRest.verbose_msg);
    for LScan in LRest.scans do
    begin
      Writeln('   ', LScan.Key, ' - ', LScan.Value.Result);
    end;
  finally
    VT.Free;
  end;
end;

procedure TestFileScan;
var
  VT: TVirusTotal;
  LRest: IvtScanResponse;
begin
  VT := TVirusTotal.Create(nil);
  VT.ApiKey := API_KEY;
  try
    LRest := VT.FileScan(TFileToSend.FromFile('C:\Users\Maxim\Desktop\Video File_48px.png'));
    Writeln(LRest.verbose_msg);
    Writeln(LRest.permalink);
  finally
    VT.Free;
  end;
end;

procedure TestFileScanUploadUrl;
var
  VT: TVirusTotal;
begin
  VT := TVirusTotal.Create(nil);
  VT.ApiKey := API_KEY;
  VT.Logger := nil;
  try
    Writeln(VT.FileScanUploadUrl);
  finally
    VT.Free;
  end;
end;

begin
  try
    TestFileScanUploadUrl;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.ToString);
  end;

  Readln;
end.

