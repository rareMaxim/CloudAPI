unit CloudAPI.Core.RequestBuilder;

interface

uses
  CloudAPI.Client.Base,
  System.Net.HttpClient,
  CloudAPI.Request,
  System.Net.Mime,
  System.Net.URLClient,
  System.Classes;

type
  TRequestBuilder = class
  private
    FClient: TCloudApiClientBase;
    FRequest: IHTTPRequest;
    FcaRequest: IcaRequest;
    FFormData: TMultipartFormData;
    FRequestBody: TStringStream;
    FUrl: TURI;
    FUrlString: string;
  protected
    procedure BuildHttpHeaders;
    procedure BuildCookies;
    procedure BuildQueryParameters;
    procedure BuildUrlSegments;
    procedure BuildGetOrPosts;
    procedure BuildFiles;
    procedure BuildFormData;
    procedure BuildRequestBody;
    function DoBuild: IHTTPRequest;
  public
    constructor Create(AClient: TCloudApiClientBase; ARequest: IcaRequest);
    class function Build(AClient: TCloudApiClientBase; ARequest: IcaRequest): IHTTPRequest;
    destructor Destroy; override;
    property UrlString: string read FUrlString;
  end;

implementation

uses
  CloudAPI.Parameter,
  CloudAPI.Types,
  System.NetEncoding,
  System.Rtti,
  System.SysUtils;

{ TRequestBuilder }

function TRequestBuilder.DoBuild: IHTTPRequest;
var
  LMethodString: string;
begin
  LMethodString := TRttiEnumerationType.GetName<TcaMethod>(FcaRequest.Method);
  if FcaRequest.Resource.IsEmpty then
    FUrl := TURI.Create(FClient.BaseUrl)
  else
    FUrl := TURI.Create(FClient.BaseUrl + '/' + FcaRequest.Resource);
  BuildUrlSegments();
  BuildGetOrPosts;
  BuildQueryParameters;
  FRequest := FClient.HttpClient.GetRequest(LMethodString, FUrl);
  BuildFiles;

  BuildHttpHeaders;
  BuildCookies;
  if FcaRequest.IsMultipartFormData then
    BuildFormData
  else
    BuildRequestBody;
  FUrlString := FUrl.ToString;
  Result := FRequest;
end;

class function TRequestBuilder.Build(AClient: TCloudApiClientBase; ARequest: IcaRequest): IHTTPRequest;
var
  MyClass: TRequestBuilder;
begin
  MyClass := TRequestBuilder.Create(AClient, ARequest);
  try
    Result := MyClass.DoBuild;
  finally
    MyClass.Free;
  end;
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
        FFormData.AddFile(LFile.FileName, LFile.Data);
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
      FUrl.AddParameter(LParam.Name, LParam.ValueAsString);
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
    FUrl.AddParameter(LParam.Name, LParam.ValueAsString);
  end;
end;

procedure TRequestBuilder.BuildRequestBody;
begin
  FRequest.SourceStream := FRequestBody;
end;

procedure TRequestBuilder.BuildUrlSegments;
var
  LFullUrl: string;
  LParam: TcaParameter;
begin
  LFullUrl := TNetEncoding.URL.Decode(FUrl.ToString);
  for LParam in FcaRequest.UrlSegments do
  begin
    LFullUrl := LFullUrl.Replace('{' + LParam.Name + '}', LParam.ValueAsString);
  end;
  FUrl := TURI.Create(LFullUrl);
end;

constructor TRequestBuilder.Create(AClient: TCloudApiClientBase; ARequest: IcaRequest);
begin
  FClient := AClient;
  FcaRequest := ARequest;
  if FcaRequest.IsMultipartFormData then
    FFormData := TMultipartFormData.Create
  else if FcaRequest.IsRequestBody then
    FRequestBody := TStringStream.Create(FcaRequest.RequestBody.Text);
end;

destructor TRequestBuilder.Destroy;
begin
  // if FcaRequest.IsMultipartFormData then
  // FFormData.Free
  // else if FcaRequest.IsRequestBody then
  // FRequestBody.Free;
  inherited Destroy;
end;

end.
