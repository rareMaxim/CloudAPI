unit pipedreamTest;

interface

uses
  DUnitX.TestFramework,
  CloudAPI.Client.Sync;

type

  [TestFixture]
  TMyTestObject = class
  strict private
    FCloud: TCloudApiClient;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Test1;
  end;

implementation

uses
  CloudAPI.Request,
  CloudAPI.Types,
  CloudAPI.Response;

procedure TMyTestObject.Setup;
begin
  FCloud := TCloudApiClient.Create();
  FCloud.BaseUrl := 'https://22791691853624b16004d88c83a8fff1.m.pipedream.net';
end;

procedure TMyTestObject.TearDown;
begin
  FCloud.Free;
end;

procedure TMyTestObject.Test1;
var
  LGet: IcaRequest;
  LResponse: IcaResponseBase;
begin
  LGet := TcaRequest.Create;
  LGet.Method := TcaMethod.GET;
  LGet.Resource := 'getCoffee';
  LGet.RequestBody.Text := '{"suggar":1}';
  LResponse := FCloud.Execute(LGet);
  Assert.AreEqual(200, LResponse.HttpResponse.StatusCode);
end;

initialization

TDUnitX.RegisterTestFixture(TMyTestObject);

end.
