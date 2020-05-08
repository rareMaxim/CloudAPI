unit CloudAPI.Core.RequestBuilder;

interface

uses
  CloudAPI.Client.Base,
  System.Net.HttpClient,
  CloudAPI.Request,
  System.Net.Mime,
  System.Net.URLClient;

type
  IcaRequestBuilder = interface
    ['{98D49AA1-417F-483D-8EBA-0708DDDFA232}']
    function Build: IHTTPRequest;
  end;

  TRequestBuilder = class(tinterfacedObject, IcaRequestBuilder)
  private
    FClient: TCloudApiClientBase;
    FRequest: IHTTPRequest;
    FcaRequest: IcaRequest;
    FFormData: TMultipartFormData;
  protected
    procedure BuildHttpHeaders;
    procedure BuildCookies;
    procedure BuildQueryParameters;
    function BuildUrlSegments(const ABaseUrl: string): TURI;
    procedure BuildGetOrPosts;
    procedure BuildFiles;
    procedure BuildFormData;
    procedure BuildRequestBody;
  public
    constructor Create(AClient: TCloudApiClientBase; ARequest: IcaRequest);
    function Build: IHTTPRequest;
    destructor Destroy; override;
  end;

implementation

uses
  CloudAPI.Parameter,
  CloudAPI.Types,
  System.Rtti,
  System.SysUtils, System.Classes;

{ TRequestBuilder }

function TRequestBuilder.Build: IHTTPRequest;
var
  LMethodString: string;
  LUrl: TURI;
begin
  LMethodString := TRttiEnumerationType.GetName<TcaMethod>(FcaRequest.Method);
  LUrl := BuildUrlSegments(FClient.BaseUrl);
  FRequest := FClient.HttpClient.GetRequest(LMethodString, LUrl);
  BuildFiles;
  BuildGetOrPosts;
  BuildQueryParameters;
  BuildHttpHeaders;
  BuildCookies;
  if FcaRequest.IsMultipartFormData then
    BuildFormData
  else
    BuildRequestBody;
  Result := FRequest;
end;

procedure TRequestBuilder.BuildCookies;
var
  LParam: TcaParameter;
  LCookie: TCookie;
begin
  for LParam in FcaRequest.Cookies do
  begin
    { TODO -oMaxim Sysoev -cGeneral : Протестировать куки }
    LCookie.Name := LParam.Name;
    LCookie.Value := LParam.ValueAsString;
    FClient.HttpClient.CookieManager.AddServerCookie(LCookie, FRequest.URL);
  end;
end;

procedure TRequestBuilder.BuildFiles;
var
  LFile: TcaFileToSend;
begin
  for LFile in FcaRequest.Files do
    case LFile.Tag of
      TcaFileToSendTag.FromFile:
        FFormData.AddFile(LFile.Name, LFile.Data);
      TcaFileToSendTag.FromStream:
        FFormData.AddStream(LFile.Name, LFile.Content, LFile.Data);
    end;
end;

procedure TRequestBuilder.BuildFormData;
begin
  FFormData.Stream.Position := 0;
  FRequest.SourceStream := FFormData.Stream;
  FRequest.AddHeader('Content-Type', FFormData.MimeTypeHeader);
end;

procedure TRequestBuilder.BuildGetOrPosts;
var
  LParam: TcaParameter;
begin
  for LParam in FcaRequest.GetOrPosts do
  begin
    if FcaRequest.IsMultipartFormData then
    begin
      FFormData.AddField(LParam.Name, LParam.ValueAsString);
    end
    else
    begin
      FRequest.URL.AddParameter(LParam.Name, LParam.ValueAsString);
    end;
  end;
end;

procedure TRequestBuilder.BuildHttpHeaders;
var
  LParam: TcaParameter;
begin
  for LParam in FcaRequest.HttpHeaders do
  begin
    FRequest.AddHeader(LParam.Name, LParam.ValueAsString);
  end;
end;

procedure TRequestBuilder.BuildQueryParameters;
var
  LParam: TcaParameter;
begin
  for LParam in FcaRequest.QueryParameters do
  begin
    FRequest.URL.AddParameter(LParam.Name, LParam.ValueAsString);
  end;
end;

procedure TRequestBuilder.BuildRequestBody;
begin
  FRequest.SourceStream := TStringStream.Create(FcaRequest.RequestBody.Text);
end;

function TRequestBuilder.BuildUrlSegments(const ABaseUrl: string): TURI;
var
  LFullUrl: string;
  LParam: TcaParameter;
begin
  LFullUrl := ABaseUrl + '/' + FcaRequest.Resource;
  for LParam in FcaRequest.UrlSegments do
  begin
    LFullUrl := LFullUrl.Replace('{' + LParam.Name + '}', LParam.ValueAsString);
  end;
  Result := TURI.Create(LFullUrl);
end;

constructor TRequestBuilder.Create(AClient: TCloudApiClientBase; ARequest: IcaRequest);
begin
  FClient := AClient;
  FcaRequest := ARequest;
  if FcaRequest.IsMultipartFormData then
    FFormData := TMultipartFormData.Create;
end;

destructor TRequestBuilder.Destroy;
begin
  if FcaRequest.IsMultipartFormData then
    FFormData.Free;
  inherited;
end;

end.
