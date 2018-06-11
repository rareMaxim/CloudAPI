unit Api.Request.Test;

interface

uses
  CloudAPI.Request,
  DUnitX.TestFramework;

type
  [TestFixture]
  TMyTestObject = class(TObject)
  strict private
    FApiRequest: TApiRequest;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    [TestCase('Value=Default, NotRequired', 'value,value,false,false')]
    [TestCase('Value<>Default, NotRequired', 'value,other,false,true')]
    [TestCase('Value<>Default, Required', 'value,other,true,true')]
    procedure NeedAdd(const AValue, ADefaultValue: string; const ARequired,
      AExpected: Boolean);
    [Test]
    [TestCase('Value=Default, Required', 'value,value,true,true')]
    [TestCase('Value<>Default, Required', 'value,other,true,false')]
    procedure RaiseArgument(const AValue, ADefaultValue: string; const ARequired,
      AExpected: Boolean);
  end;

implementation

procedure TMyTestObject.RaiseArgument(const AValue, ADefaultValue: string; const
  ARequired, AExpected: Boolean);
begin
  Assert.AreEqual(AExpected, FApiRequest.RaiseArgument(AValue, ADefaultValue, ARequired));
end;

procedure TMyTestObject.Setup;
begin
  FApiRequest := TApiRequest.Create;
end;

procedure TMyTestObject.TearDown;
begin
  FApiRequest.Free;
  FApiRequest := nil;
end;

procedure TMyTestObject.NeedAdd(const AValue, ADefaultValue: string; const
  ARequired, AExpected: Boolean);
begin
  Assert.AreEqual(AExpected, FApiRequest.NeedAdd(AValue, ADefaultValue, ARequired));
end;

initialization
  TDUnitX.RegisterTestFixture(TMyTestObject);

end.

