unit CloudAPI.Ext.MethodLimits;

interface

uses
  CloudAPI.Types,
  System.Generics.Collections,
  System.SysUtils;

type

  TcaRequestLimitList = class(TList<TcaRequestLimit>);

  TcaRequestLimitManager = class
  private
    FGlobal: TcaRequestLimitList;
    FLocal: TcaRequestLimitList;
    FOnLimit: TProc<Int64>;
  protected
    function CalculateWait(AList: TcaRequestLimitList; const AName: string = ''): Int64;
  public
    constructor Create;
    procedure Add(const ALimit: Int64; const AName: string; const AIsGlobal: Boolean);
    function GetGlobalLimits: TArray<TcaRequestLimit>;
    function GetLocalLimits(const AName: string): TArray<TcaRequestLimit>;
    function GetLimits(const AName: string): TArray<TcaRequestLimit>;
    function GlobalWait: Int64;
    function LocalWait(const AName: string): Int64;
    destructor Destroy; override;
    property OnLimit: TProc<Int64> read FOnLimit write FOnLimit;
  end;

implementation

{ TcaRequestLimitManager }

procedure TcaRequestLimitManager.Add(const ALimit: Int64; const AName: string; const AIsGlobal: Boolean);
var
  LLimit: TcaRequestLimit;
begin
  LLimit := TcaRequestLimit.Create(ALimit, AName, AIsGlobal);
  if AIsGlobal then
    FGlobal.Add(LLimit)
  else
    FLocal.Add(LLimit);
end;

function TcaRequestLimitManager.CalculateWait(AList: TcaRequestLimitList; const AName: string = ''): Int64;
var
  I: Integer;
begin
  Result := 0;
  for I := AList.Count - 1 downto 0 do
  begin
    if AList.Count = 0 then
      break;
    if AList[I].IsExpired then
      AList.Delete(I)
    else if AList[I].Name.Contains(AName) then
      Inc(Result, AList[I].ActualLimit);
  end;
end;

constructor TcaRequestLimitManager.Create;
begin
  inherited Create;
  FGlobal := TcaRequestLimitList.Create;
  FLocal := TcaRequestLimitList.Create;
end;

destructor TcaRequestLimitManager.Destroy;
begin
  FGlobal.Free;
  FLocal.Free;
  inherited Destroy;
end;

function TcaRequestLimitManager.GetGlobalLimits: TArray<TcaRequestLimit>;
begin
  CalculateWait(FGlobal, '');
  Result := FGlobal.ToArray;
end;

function TcaRequestLimitManager.GetLimits(const AName: string): TArray<TcaRequestLimit>;
begin
  CalculateWait(FGlobal, '');
  CalculateWait(FLocal, '');
  Result := FLocal.ToArray;
end;

function TcaRequestLimitManager.GetLocalLimits(const AName: string): TArray<TcaRequestLimit>;
begin
  CalculateWait(FLocal, AName);
  Result := FLocal.ToArray;
end;

function TcaRequestLimitManager.GlobalWait: Int64;
begin
  Result := CalculateWait(FGlobal);
end;

function TcaRequestLimitManager.LocalWait(const AName: string): Int64;
begin
  Result := GlobalWait + CalculateWait(FLocal, AName);
end;

end.
