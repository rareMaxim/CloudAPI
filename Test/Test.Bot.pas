unit Test.Bot;

interface

uses
  TelegAPI.Bot,
  TelegAPI.Types,
  DUnitX.TestFramework;

type
  [TestFixture]
  TBotMethods = class(TObject)
  private
    FBot: TTelegramBot;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    [TestCase('', '{"ok":true,"result":{"id":283107814,"is_bot":true,"first_name":"1","username":"MyTestDelphiBot"}}', '')]
    procedure GetMe(const ARequest: string);
  end;

implementation

procedure TBotMethods.GetMe(const ARequest: string);
var
  LActualUser, LExcepted: TtgUser;
begin
  LExcepted := TtgUser.Create;
  try
    LExcepted.ID := 283107814;
    LExcepted.IsBot := True;
    LExcepted.FirstName := '1';
    LExcepted.Username := 'MyTestDelphiBot';
    LActualUser := FBot.ApiTest<TtgUser>(ARequest);
    Assert.AreEqual(LExcepted, LActualUser);
  finally
    LExcepted.Free;
    LActualUser.Free;
  end;
end;

procedure TBotMethods.Setup;
begin
  FBot := TTelegramBot.Create(nil);
end;

procedure TBotMethods.TearDown;
begin
  FBot.Free;
end;

initialization
  TDUnitX.RegisterTestFixture(TBotMethods);

end.

