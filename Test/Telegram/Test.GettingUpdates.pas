unit Test.GettingUpdates;

interface

uses
  TelegAPI.Bot,
  TelegAPI.Bot.Impl,
  TelegAPI.Types,
  DUnitX.TestFramework;

type
  [TestFixture]
  TGettingUpdatesTests = class(TObject)
  strict private
    FBot: ITelegramBot;
  public
    function TestApi(const AToken: string): Boolean;
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestApiToken;
    [Test]
    procedure ApiTokenBad;
    [Test]
    procedure GetBotUser;
  end;

implementation

uses
  Test.Config,
  System.SysUtils,
  CloudAPI.Request;

procedure TGettingUpdatesTests.TestApiToken;
var
  LResult: Boolean;
begin
  LResult := TestApi(TestConfig.Token);
  Assert.IsTrue(LResult);
end;

procedure TGettingUpdatesTests.ApiTokenBad;
var
  LResult: Boolean;
begin
  try
    LResult := TestApi('0:1this_is_an-invalid-token_for_tests');
    Assert.IsFalse(LResult);
  except
    on E: ECloudApiException do
    begin
      Assert.AreEqual('404', E.Code);
    end;
  end;
end;

procedure TGettingUpdatesTests.GetBotUser;
var
  LUser: ItgUser;
begin
  LUser := FBot.GetMe;
  Assert.IsNotNull(LUser);
  Assert.IsTrue(LUser.IsBot);
  Assert.EndsWith('bot', LUser.Username);
end;

procedure TGettingUpdatesTests.Setup;
begin
  FBot := TTelegramBot.Create(TestConfig.Token);
end;

procedure TGettingUpdatesTests.TearDown;
begin
  (FBot as TTelegramBot).Free;
  FBot := nil;
end;

function TGettingUpdatesTests.TestApi(const AToken: string): Boolean;
var
  LBot: ITelegramBot;
begin
  LBot := TTelegramBot.Create(AToken);
  Result := True;
  try
    LBot.GetMe;
  except
    on E: ECloudApiException do
      Result := False;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TGettingUpdatesTests);

end.

