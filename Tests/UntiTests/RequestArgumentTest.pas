unit RequestArgumentTest;

interface

uses
  CloudAPI.RequestArgument,
  CloudAPI.Parameter,
  System.Rtti,
  DUnitX.TestFramework;

type

  [TestFixture]
  TcaRequestArgumentTest = class
  strict private
    fRequestArgument: TcaRequestArgument;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure RecordOneValue;
    [Test]
    procedure RecordTwoValue;
    [Test]
    procedure ClassOneValue;
    [Test]
    procedure ClassTwoValue;
  end;

implementation

uses
  RequestArgumentTest.Types;

procedure TcaRequestArgumentTest.RecordTwoValue;
var
  lParams: TArray<TcaParameter>;
  lVal: TTwoPairRecord;
begin
  lVal := TTwoPairRecord.GetValue;
  lParams := fRequestArgument.ObjToParams<TTwoPairRecord>(lVal);
  Assert.IsNotNull(lParams);
  Assert.AreEqual(lParams[0].Name, 'key1');
  Assert.AreEqual(lParams[0].ValueAsString, 'Value1');
  Assert.AreEqual(lParams[1].Name, 'key2');
  Assert.AreEqual(lParams[1].ValueAsString, 'Value2');
end;

procedure TcaRequestArgumentTest.Setup;
begin
  fRequestArgument := TcaRequestArgument.Create;
end;

procedure TcaRequestArgumentTest.TearDown;
begin
  fRequestArgument.Free;
  fRequestArgument := nil;
end;

procedure TcaRequestArgumentTest.ClassOneValue;
var
  lParams: TArray<TcaParameter>;
  lVal: TOnePairClass;
begin
  lVal := TOnePairClass.GetValue;
  try
    lParams := fRequestArgument.ObjToParams<TOnePairClass>(lVal);
    Assert.IsNotNull(lParams);
    Assert.AreEqual(1, Length(lParams));
    Assert.AreEqual(lParams[0].Name, 'key');
    Assert.AreEqual(lParams[0].ValueAsString, 'Value');
  finally
    lVal.Free;
  end;
end;

procedure TcaRequestArgumentTest.ClassTwoValue;
var
  lParams: TArray<TcaParameter>;
  lVal: TTwoPairClass;
begin
  lVal := TTwoPairClass.GetValue;
  try
    lParams := fRequestArgument.ObjToParams<TTwoPairClass>(lVal);
    Assert.IsNotNull(lParams);
    Assert.AreEqual(2, Length(lParams));
    Assert.AreEqual(lParams[0].Name, 'key2');
    Assert.AreEqual(lParams[0].ValueAsString, 'Value');
    Assert.AreEqual(lParams[1].Name, 'key1');
    Assert.AreEqual(lParams[1].ValueAsString, '1');
  finally
    lVal.Free;
  end;
end;

procedure TcaRequestArgumentTest.RecordOneValue;
var
  lParams: TArray<TcaParameter>;
  lVal: TOnePairRecord;
begin
  lVal := TOnePairRecord.GetValue;
  lParams := fRequestArgument.ObjToParams<TOnePairRecord>(lVal);
  Assert.IsNotNull(lParams);
  Assert.AreEqual(lParams[0].Name, 'key');
  Assert.AreEqual(lParams[0].ValueAsString, 'Value');
end;

initialization

TDUnitX.RegisterTestFixture(TcaRequestArgumentTest);

end.
