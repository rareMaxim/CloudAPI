unit Test.Bot;

interface

uses
  TelegAPI.Bot,
  TelegAPI.Types,
  DJSON.Params,
  DUnitX.TestFramework;

type
  [TestFixture]
  TBotMethods = class(TObject)
  private
    FBot: TTelegramBot;
    FParams: IdjParams;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    [TestCase('', '{"ok":true,"result":{"id":283107814,"is_bot":true,"first_name":"1","username":"MyTestDelphiBot"}}', '')]
    procedure GetMe(const ARequest: string);
  end;

implementation

uses
  DJSON;

procedure TBotMethods.GetMe(const ARequest: string);
var
  LActualUser, LExcepted: TtgUser;
  x, y: string;
begin
  LExcepted := TtgUser.Create;
  try
    LExcepted.ID := 283107814;
    LExcepted.IsBot := True;
    LExcepted.FirstName := '1';
    LExcepted.Username := 'MyTestDelphiBot';
    LActualUser := FBot.ApiTest<TtgUser>(ARequest);
    x := dj.From(LExcepted, FParams).ToJson;
    y := dj.From(LActualUser, FParams).ToJson;
    Assert.AreEqual(x, y);
  finally
    LExcepted.Free;
    LActualUser.Free;
  end;
end;

procedure TBotMethods.Setup;
begin
  FBot := TTelegramBot.Create(nil);
  FParams := dj.DefaultByFields;
  FParams.Engine := eDelphiDOM;
end;

procedure TBotMethods.TearDown;
begin
  FBot.Free;
end;

initialization
  TDUnitX.RegisterTestFixture(TBotMethods);

end.

