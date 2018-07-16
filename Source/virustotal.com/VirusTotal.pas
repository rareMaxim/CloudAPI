unit VirusTotal;

interface

uses
  CloudAPI.BaseComponent,
  CloudAPI.Types,
  CloudAPI.Request,
  VirusTotal.Types,
  System.Classes;

type
  TVirusTotal = class(TCloudApiBaseComponent)
  private
    FApiKey: string;
  protected
    procedure DoInitApiCore; override;
  public
    function FileReport(const AResource: string; const AAllInfo: Boolean = False): IvtReport;
    function FileScan(const AFile: TFileToSend): IvtScanResponse;
    function FileScanUploadUrl: string;
    function FileRescan(const AResource: string): IvtScanResponse;
    procedure FileDownload(const AHash: string; var AStream: TStream);
    function FileBehaviour(const AHash: string): INeedPrivateApi;
    function FileNetworkTraffic(const AHash: string): INeedPrivateApi;
    function FileFeed(const APackage: string): INeedPrivateApi;
    function FileClusters(const Date: TDate): INeedPrivateApi;
    function FileFalsePositives(const ALimit: Integer = 100): INeedPrivateApi;
    function FileSearch(const AQuery: string; const AOffset: string = ''): INeedPrivateApi;
    function UrlReport(const AResource: string; const AAllInfo: Boolean = False; const AScan: Integer = 0): IvtReport;
    function UrlScan(const AUrl: string): IvtScanResponse;
    function UrlFeed(const APackage: string): INeedPrivateApi;
  published
    property ApiKey: string read FApiKey write FApiKey;
  end;

  TFileToSend = CloudAPI.Types.TFileToSend;

implementation

uses
  System.SysUtils;
{ TVirusTotal }

procedure TVirusTotal.DoInitApiCore;
begin
  inherited;
  Domain := 'https://www.virustotal.com/vtapi/v2';
  GetRequest.StoreAutoFormat := TStoreFormat.InUrl;
  GetRequest.OnStaticFill :=
    procedure
    begin
      GetRequest.AddParameter('apikey', ApiKey, '', True);
    end;
end;

function TVirusTotal.FileBehaviour(const AHash: string): INeedPrivateApi;
begin
  with GetRequest do
  begin
    SetMethod('file/behaviour');
    AddParameter('hash', AHash, '', True);
  end;
end;

function TVirusTotal.FileClusters(const date: TDate): INeedPrivateApi;
begin
  with GetRequest do
  begin
    SetMethod('file/clusters');
    AddParameter('date', FormatDateTime('YYYY-MM-DD ', date), '', True);
  end;
end;

procedure TVirusTotal.FileDownload(const AHash: string; var AStream: TStream);
begin
  with GetRequest do
  begin
    SetMethod('file/download');
    AddParameter('hash', AHash, '', True);
    AStream := Execute.ContentStream;
  end;
end;

function TVirusTotal.FileFalsePositives(const ALimit: Integer): INeedPrivateApi;
begin
  with GetRequest do
  begin
    SetMethod('file/false-positives');
    AddParameter('limit', ALimit, 100, False);
    AddParameter('Accept-Encoding', 'gzip', '', True, TStoreFormat.InHeader);
    AddParameter('User-Agent', 'gzip', '', True, TStoreFormat.InHeader);
  end;
end;

function TVirusTotal.FileFeed(const APackage: string): INeedPrivateApi;
begin
  with GetRequest do
  begin
    SetMethod('file/feed');
    AddParameter('package', APackage, '', True);
  end;
end;

function TVirusTotal.FileNetworkTraffic(const AHash: string): INeedPrivateApi;
begin
  with GetRequest do
  begin
    SetMethod('file/network-traffic');
    AddParameter('hash', AHash, '', True);
  end;
end;

function TVirusTotal.FileReport(const AResource: string; const AAllInfo: Boolean): IvtReport;
begin
  with GetRequest do
  begin
    SetMethod('file/report');
    AddParameter('resource', AResource, '', True);
    AddParameter('allinfo', AAllInfo, False, False);
    Result := TvtReport.Create(ExecuteAsString);
  end;
end;

function TVirusTotal.FileRescan(const AResource: string): IvtScanResponse;
begin
  with GetRequest do
  begin
    SetMethod('file/rescan');
    AddParameter('resource', AResource, '', True, TStoreFormat.InFormData);
    Result := TvtScanResponse.Create(ExecuteAsString);
  end;
end;

function TVirusTotal.FileScan(const AFile: TFileToSend): IvtScanResponse;
begin
  with GetRequest do
  begin
    SetMethod('file/scan');
    AddParameter('file', AFile, nil, True, TStoreFormat.InFormData);
    Result := TvtScanResponse.Create(ExecuteAsString);
  end;
end;

function TVirusTotal.FileScanUploadUrl: string;
begin
  with GetRequest do
  begin
    SetMethod('file/scan/upload_url');
    Result := ExecuteAsString;
  end;
end;

function TVirusTotal.FileSearch(const AQuery, AOffset: string): INeedPrivateApi;
begin
  with GetRequest do
  begin
    SetMethod('file/search');
    AddParameter('query', AQuery, '', True);
    AddParameter('offset', AOffset, '', False);
  end;
end;

function TVirusTotal.UrlFeed(const APackage: string): INeedPrivateApi;
begin
  with GetRequest do
  begin
    SetMethod('url/feed');
    AddParameter('package', APackage, '', True);
  end;
end;

function TVirusTotal.UrlReport(const AResource: string; const AAllInfo: Boolean; const AScan: Integer): IvtReport;
begin
  with GetRequest do
  begin
    SetMethod('url/report');
    AddParameter('resource', AResource, '', True);
    AddParameter('allinfo', AAllInfo, False, False);
    AddParameter('scan', AScan, 0, False);
    Result := TvtReport.Create(ExecuteAsString);
  end;
end;

function TVirusTotal.UrlScan(const AUrl: string): IvtScanResponse;
begin
  with GetRequest do
  begin
    SetMethod('url/scan');
    AddParameter('url', AUrl, '', True);
    Result := TvtScanResponse.Create(ExecuteAsString);
  end;
end;

end.

