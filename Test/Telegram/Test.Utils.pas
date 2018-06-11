unit Test.Utils;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTelegramTest = class(TObject)
  public
    [Test]
    procedure TypeToJArray;
  end;

implementation

uses
  TelegAPI.Utils.Json;

{ TTelegramTest }

procedure TTelegramTest.TypeToJArray;
var
  LArray: TArray<string>;
begin
  LArray := ['1', '2'];
  Assert.AreEqual('["1","2"]', TJsonUtils.ArrayToJString<string>(LArray));
end;

initialization
  TDUnitX.RegisterTestFixture(TTelegramTest);

end.

