unit CloudAPI.Core.RequestBuilder;

interface

uses
  CloudAPI.Client.Base,
  System.Net.HttpClient,
  CloudAPI.Request,
  System.Net.Mime,
  System.Net.URLClient,
  System.Classes,
  System.SysUtils,
  CloudAPI.Types;

type
  TRequestBuilder = class
  private
    FClient: TCloudApiClientBase;
    FRequest: IHTTPRequest;
    FcaRequest: IcaRequest;
    FFormData: TMultipartFormData;
    FRequestBody: TStringList;
    FUrl: TURI;
    FUrlString: string;
  protected
    procedure BuildHttpHeaders;
    procedure BuildCookies;
    procedure BuildQueryParameters;
    class function BuildUrlSegments(const AUrl: string; ARequest: IcaRequest): TURI;
    procedure BuildGetOrPosts;
    procedure BuildFiles;
    procedure BuildFormData;
    procedure BuildRequestBody;
    function DoBuild: IHTTPRequest;
    class procedure CreateFormFromStrings(const ASource: TStrings; const AEncoding: TEncoding;
      const AHeaders: TNetHeaders; var ASourceStream: TStream; var ASourceHeaders: TNetHeaders);
  public
    constructor Create(AClient: TCloudApiClientBase; ARequest: IcaRequest);
    function Build: IHTTPRequest; overload;
    class function Build(AClient: TCloudApiClientBase; ARequest: IcaRequest): IHTTPRequest; overload;
    destructor Destroy; override;
    property UrlString: string read FUrlString;
  end;

implementation

uses
  CloudAPI.Parameter,
  System.NetEncoding,
  System.Rtti;

{ TRequestBuilder }

function TRequestBuilder.DoBuild: IHTTPRequest;
var
  LMethodString: string;
begin
  LMethodString := TRttiEnumerationType.GetName<TcaMethod>(FcaRequest.Method);
  if FcaRequest.Resource.IsEmpty then
    FUrl := BuildUrlSegments(FClient.BaseUrl, FcaRequest)
  else if FClient.BaseUrl.IsEmpty then
    FUrl := BuildUrlSegments(FcaRequest.Resource, FcaRequest)
  else
    FUrl := BuildUrlSegments(FClient.BaseUrl + '/' + FcaRequest.Resource, FcaRequest);

  BuildGetOrPosts;
  BuildQueryParameters;
  FRequest := FClient.HttpClient.GetRequest(LMethodString, FUrl);
  BuildFiles;

  BuildCookies;
  if FcaRequest.IsMultipartFormData then
    BuildFormData
  else
    BuildRequestBody;
  BuildHttpHeaders;
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
    case LFile.&Type of
      TcaFileToSendType.File:
        FFormData.AddFile(LFile.Name, LFile.FilePath);
      TcaFileToSendType.Stream:
        FFormData.AddStream(LFile.Name, LFile.Content, LFile.FilePath);
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
    if FcaRequest.Method = TcaMethod.POST then
    begin
      if FcaRequest.IsMultipartFormData then
      begin
        FFormData.AddField(LParam.Name, LParam.ValueAsString);
      end
      else
      begin
        FRequestBody.Add(LParam.Name + '=' + LParam.ValueAsString)
      end;
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
    FRequest.HeaderValue[LParam.Name] := LParam.ValueAsString;
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
var
  LSourceHeaders: TNetHeaders;
  LSourceStream: TStream;
  I: Integer;
begin
  LSourceHeaders := [];
  CreateFormFromStrings(FRequestBody, TEncoding.UTF8, FRequest.Headers, LSourceStream, LSourceHeaders);
  for I := Low(LSourceHeaders) to High(LSourceHeaders) do
    FcaRequest.HttpHeaders.Add(TcaParameter.Create(LSourceHeaders[I].Name, LSourceHeaders[I].Value, '',
      TcaParameterType.HttpHeader, False));
  FRequest.SourceStream := LSourceStream;
end;

class function TRequestBuilder.BuildUrlSegments(const AUrl: string; ARequest: IcaRequest): TURI;
var
  LFullUrl: string;
  LParam: TcaParameter;
begin
  LFullUrl := AUrl;
  for LParam in ARequest.UrlSegments do
    LFullUrl := LFullUrl.Replace('{' + LParam.Name + '}', LParam.ValueAsString);
  Result := TURI.Create(LFullUrl);
end;

constructor TRequestBuilder.Create(AClient: TCloudApiClientBase; ARequest: IcaRequest);
begin
  FClient := AClient;
  FcaRequest := ARequest;
  if FcaRequest.IsMultipartFormData then
    FFormData := TMultipartFormData.Create
  else
  begin
    FRequestBody := TStringList.Create;
    FRequestBody.LineBreak := '&';
  end;
end;

class procedure TRequestBuilder.CreateFormFromStrings(const ASource: TStrings; const AEncoding: TEncoding;
  const AHeaders: TNetHeaders; var ASourceStream: TStream; var ASourceHeaders: TNetHeaders);
var
  lHttp: THTTPClient;
begin
  lHttp := THTTPClient.Create;
  try
    lHttp.CreateFormFromStrings(ASource, AEncoding, AHeaders, ASourceStream, ASourceHeaders);
  finally
    lHttp.Free;
  end;
end;

destructor TRequestBuilder.Destroy;
begin
  FRequestBody.Free;
  inherited Destroy;
end;

function TRequestBuilder.Build: IHTTPRequest;
begin
  Result := DoBuild;
end;

end.
