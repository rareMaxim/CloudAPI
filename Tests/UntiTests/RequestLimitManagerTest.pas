unit RequestLimitManagerTest;

interface

uses
  CloudApi.Ext.MethodLimits,
  DUnitX.TestFramework;

type

  [TestFixture]
  TRequestLimitManagerTest = class
  strict private
    FLimitMng: TcaRequestLimitManager;
  private

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure LocalLimit;
    [Test]
    procedure GlobalLimit;
  end;

implementation

uses
  System.SysUtils;

procedure TRequestLimitManagerTest.GlobalLimit;
const
  LIMIT_NAME = 'GLOBAL';
var
  LLimit: Int64;
begin
  FLimitMng.Add(1000, LIMIT_NAME, True);
  LLimit := FLimitMng.GlobalWait();
  Assert.IsTrue(LLimit > 900);
  sleep(500);
  LLimit := FLimitMng.GlobalWait;
  Assert.IsTrue(LLimit < 500);

end;

procedure TRequestLimitManagerTest.LocalLimit;
const
  LIMIT_NAME = 'loc_lim_1000';
var
  LLimit: Int64;
begin
  FLimitMng.Add(1000, LIMIT_NAME, False);
  LLimit := FLimitMng.LocalWait(LIMIT_NAME);
  Assert.IsTrue(LLimit > 900);
  sleep(500);
  LLimit := FLimitMng.LocalWait(LIMIT_NAME);
  Assert.IsTrue(LLimit < 500);
end;

procedure TRequestLimitManagerTest.Setup;
begin
  FLimitMng := TcaRequestLimitManager.Create;
end;

procedure TRequestLimitManagerTest.TearDown;
begin
  FLimitMng.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TRequestLimitManagerTest);

end.
