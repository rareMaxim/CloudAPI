unit InvisionCommunity;

interface

uses
  CloudApi.Exception,
  System.SysUtils,
  InvisionCommunity.System,
  InvisionCommunity.Forums;

type
  TInvComm = class
  private
    FForums: TicForums;
    FToken: string;
    FUrl: string;
    FSystem: TicSystem;
    FOnError: TOnError;
    procedure SetUrl(const Value: string);
    procedure SetToken(const Value: string);
    procedure SetOnError(const Value: TOnError);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property System: TicSystem read FSystem write FSystem;
    property Forums: TicForums read FForums write FForums;
    property Token: string read FToken write SetToken;
    property Url: string read FUrl write SetUrl;
    property OnError: TOnError read FOnError write SetOnError;
  end;

implementation

{ TInvComm }

constructor TInvComm.Create;
begin
  FSystem := TicSystem.Create(nil);
  FForums := TicForums.Create(nil);
end;

destructor TInvComm.Destroy;
begin
  FSystem.Free;
  FForums.Free;
  inherited;
end;

procedure TInvComm.SetOnError(const Value: TOnError);
begin
  FOnError := Value;
  FSystem.OnError := Value;
  FForums.OnError := Value;
end;

procedure TInvComm.SetToken(const Value: string);
begin
  FToken := Value;
  FSystem.Token := Value;
  FForums.Token := Value;
end;

procedure TInvComm.SetUrl(const Value: string);
begin
  FUrl := Value;
  FSystem.Url := Value;
  FForums.Url := Value;
end;

end.
