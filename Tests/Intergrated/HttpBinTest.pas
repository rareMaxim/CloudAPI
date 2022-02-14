unit HttpBinTest;

interface

uses
  CloudApi.Client.Sync,
  DUnitX.TestFramework,
  System.Generics.Collections,
  CloudApi.Types;

type
  TBaseTest = class
    procedure PrintDict(NewParam: TArray < TPair < string, string >> );
  end;

  [TestFixture]
  THTTPMethodsTest = class(TBaseTest)
  strict private
    FCloud: TCloudApiClient;
  protected
    procedure InternalExec(const AMethod: TcaMethod; const AResource: string; AWithPrint: Boolean);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Get;
    [Test]
    procedure Post;
    [Test]
    procedure Delete;
    [Test]
    procedure Patch;
    [Test]
    procedure PUT;
    [Test]
    procedure Image;
  end;

  [TestFixture]
  TAuthTest = class(TBaseTest)
  strict private
    FCloud: TCloudApiClient;
  protected
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure BasicAuth;
  end;

implementation

uses
  CloudApi.Authenticator.Basic,
  CloudApi.Response,
  CloudApi.Request,
  HttpBinTest.Types;

procedure THTTPMethodsTest.Patch;
begin
  InternalExec(TcaMethod.Patch, 'patch', False);
end;

procedure THTTPMethodsTest.Post;
begin
  InternalExec(TcaMethod.Post, 'post', False);
end;

procedure THTTPMethodsTest.PUT;
begin
  InternalExec(TcaMethod.PUT, 'put', False);
end;

procedure THTTPMethodsTest.Setup;
begin
  FCloud := TCloudApiClient.Create;
  FCloud.BaseUrl := 'https://httpbin.org';
end;

procedure THTTPMethodsTest.TearDown;
begin
  FCloud.Free;
end;

procedure THTTPMethodsTest.Delete;
begin
  InternalExec(TcaMethod.Delete, 'delete', False);
end;

procedure THTTPMethodsTest.Get;
begin
  InternalExec(TcaMethod.Get, 'get', False);
end;

procedure THTTPMethodsTest.Image;
var
  LRequest: IcaRequest;
  LResp: IcaResponseBase;
begin
  LRequest := TcaRequest.Create;
  LRequest.AddHeader('accept', 'image/*');
  LResp := FCloud.Download('https://httpbin.org/image', '1.png', LRequest);
  Assert.AreEqual(200, LResp.HttpResponse.StatusCode);
end;

procedure THTTPMethodsTest.InternalExec(const AMethod: TcaMethod; const AResource: string; AWithPrint: Boolean);
var
  LReq: IcaRequest;
  LRes: IcaResponse<TTestResponseBody>;
begin
  LReq := TcaRequest.Create;
  LReq.Resource := AResource;
  LReq.Method := AMethod;
  LRes := FCloud.Execute<TTestResponseBody>(LReq);
  Assert.AreEqual(LRes.HttpResponse.StatusCode, 200);
  Assert.IsNotEmpty(LRes.Data.Url);
  if AWithPrint then
    PrintDict(LRes.Data.Headers.ToArray);
end;

{ TAuthTest }

procedure TAuthTest.BasicAuth;
const
  AUTH_USERNAME = 'username';
  AUTH_PASSWORD = 'passWord';
var
  LReq: IcaRequest;
  LRes: IcaResponseBase;
begin
  FCloud.Authenticator := TBasicAuthenticator.Create(AUTH_USERNAME, AUTH_PASSWORD);
  LReq := TcaRequest.Create;
  LReq.Resource := 'basic-auth/' + AUTH_USERNAME + '/' + AUTH_PASSWORD;
  LReq.Method := TcaMethod.Get;
  LRes := FCloud.Execute(LReq);
  Assert.AreEqual(LRes.HttpResponse.StatusCode, 200);
end;

procedure TAuthTest.Setup;
begin
  FCloud := TCloudApiClient.Create;
  FCloud.BaseUrl := 'https://httpbin.org';
end;

procedure TAuthTest.TearDown;
begin
  FCloud.Free;
end;

{ TBaseTest }

procedure TBaseTest.PrintDict(NewParam: TArray < TPair < string, string >> );
var
  I: Integer;
begin
  System.Writeln('Count: ', Length(NewParam));
  for I := Low(NewParam) to High(NewParam) do
    System.Writeln(NewParam[I].Key + '=' + NewParam[I].Value);

end;

initialization

TDUnitX.RegisterTestFixture(THTTPMethodsTest);

end.
