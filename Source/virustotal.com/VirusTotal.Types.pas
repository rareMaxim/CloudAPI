unit VirusTotal.Types;

interface

uses
  System.Generics.Collections,
  CloudAPI.Utils.Json;

{ TODO -oOwner -cGeneral : Преобразовать время: Строка=>TDateTime }
type
  INeedPrivateApi = interface
    ['{99D9CEB7-E0ED-42F7-AE2D-279E3B6A2C7B}']
  end;
  {$REGION 'vtScan'}

  IvtScan = interface
    ['{9914B45E-23E3-4A89-8ACC-894C32009C0D}']
    function detected: Boolean;
    function version: string;
    function Result: string;
    function update: string;
  end;

  TvtScan = class(TBaseJson, IvtScan)
  public
    function detected: Boolean;
    function version: string;
    function Result: string;
    function update: string;
  end;
  {$ENDREGION}
  {$REGION 'vtScan'}

  IvtScanResponse = interface
    ['{BF88E0FF-82AB-4876-9E7F-6D2CF561595B}']
    function permalink: string;
    function resource: string;
    function response_code: Integer;
    function scan_id: string;
    function verbose_msg: string;
    function sha256: string;
  end;

  TvtScanResponse = class(TBaseJson, IvtScanResponse)
    function permalink: string;
    function resource: string;
    function response_code: Integer;
    function scan_id: string;
    function verbose_msg: string;
    function sha256: string;
  end;
  {$ENDREGION}
  {$REGION 'vtReport'}

  IvtReport = interface(IvtScanResponse)
    ['{85648437-CFE1-4B18-865D-FA9AE401403F}']
    function md5: string;
    function sha1: string;
    function scan_date: string;
    function positives: Integer;
    function total: Integer;
    function scans: TArray<TPair<string, IvtScan>>;
  end;

  TvtReport = class(TvtScanResponse, IvtReport)
    function md5: string;
    function sha1: string;
    function scan_date: string;
    function positives: Integer;
    function total: Integer;
    function scans: TArray<TPair<string, IvtScan>>;
  end;
  {$ENDREGION}

  IvtUrlSamples = interface
    ['{B804D913-F704-4B94-B874-B1B8478A3FC6}']
    function date: string;
    function positives: Integer;
    function total: Integer;
    function sha256: string;
  end;

  TvtUrlSamples = class(TBaseJson, IvtUrlSamples)
    function date: string;
    function positives: Integer;
    function total: Integer;
    function sha256: string;
  end;

  IvtDomainReport = interface
    ['{89BB4A77-F4BA-400C-945F-3282E33FE346}']
    function BitDefenderCategory: string;
    function undetected_referrer_samples: TArray<IvtUrlSamples>;
    function whois_timestamp: Int64;
    function detected_downloaded_samples: TArray<IvtUrlSamples>;
    function Malwarebytes_hpHosts_info: string;
    function detected_referrer_samples: string;
  end;

  TvtDomainReport = class(TBaseJson, IvtUrlSamples)
    function BitDefenderCategory: string;
    function undetected_referrer_samples: TArray<IvtUrlSamples>;
    function whois_timestamp: Int64;
    function detected_downloaded_samples: TArray<IvtUrlSamples>;
    function Malwarebytes_hpHosts_info: string;
    function detected_referrer_samples: string;
  end;

implementation


{ TvtScan }

function TvtScan.detected: Boolean;
begin
  Result := ToSimpleType<Boolean>('detected');
end;

function TvtScan.Result: string;
begin
  Result := ToSimpleType<string>('result');
end;

function TvtScan.update: string;
begin
  Result := ToSimpleType<string>('update');
end;

function TvtScan.version: string;
begin
  Result := ToSimpleType<string>('version');
end;

{ TvtReport }

function TvtReport.md5: string;
begin
  Result := ToSimpleType<string>('md5');
end;

function TvtReport.positives: Integer;
begin
  Result := ToSimpleType<Integer>('positives');
end;

function TvtReport.scans: TArray<TPair<string, IvtScan>>;
begin
  Result := ToPairs<IvtScan>(TvtScan, 'scans');
end;

function TvtReport.scan_date: string;
begin
  Result := ToSimpleType<string>('scan_date');
end;

function TvtReport.sha1: string;
begin
  Result := ToSimpleType<string>('sha1');
end;

function TvtReport.total: Integer;
begin
  Result := ToSimpleType<Integer>('total');
end;


{ TvtScanResponse }

function TvtScanResponse.permalink: string;
begin
  Result := ToSimpleType<string>('permalink');
end;

function TvtScanResponse.resource: string;
begin
  Result := ToSimpleType<string>('resource');
end;

function TvtScanResponse.response_code: Integer;
begin
  Result := ToSimpleType<Integer>('response_code');
end;

function TvtScanResponse.scan_id: string;
begin
  Result := ToSimpleType<string>('scan_id');
end;

function TvtScanResponse.sha256: string;
begin
  Result := ToSimpleType<string>('sha256');
end;

function TvtScanResponse.verbose_msg: string;
begin
  Result := ToSimpleType<string>('verbose_msg');
end;

{ TvtUrlSamples }

function TvtUrlSamples.date: string;
begin
  Result := ToSimpleType<string>('date');
end;

function TvtUrlSamples.positives: Integer;
begin
  Result := ToSimpleType<Integer>('positives');
end;

function TvtUrlSamples.sha256: string;
begin
  Result := ToSimpleType<string>('sha256');
end;

function TvtUrlSamples.total: Integer;
begin
  Result := ToSimpleType<Integer>('total');
end;

end.

