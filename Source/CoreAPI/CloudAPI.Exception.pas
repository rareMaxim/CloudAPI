unit CloudAPI.Exception;

interface

uses
  System.SysUtils,
  CloudAPI.Types;

type
  ECloudApiException = class(Exception)
  private
    FCode: string;
    FDescription: string;
    FRequest: IRequestData;
    FResponse: string;
    FTime: TDateTime;
  public
    constructor Create(AException: Exception); reintroduce; overload;
    constructor Create(const ACode, ADescription: string; ARequest: IRequestData = nil); reintroduce; overload;
    constructor Create(const ACode: Integer; const ADescription: string; ARequest: IRequestData = nil);
      reintroduce; overload;
    function ToString: string; override;
    property Code: string read FCode write FCode;
    property Description: string read FDescription write FDescription;
    property Request: IRequestData read FRequest write FRequest;
    property Response: string read FResponse write FResponse;
    property Time: TDateTime read FTime write FTime;
  end;

{$IFDEF CONSOLE}
  TOnError = TProc<TObject, ECloudApiException>;
{$ELSE}
  TOnError = procedure(ASender: TObject; const Exception: ECloudApiException) of object;
{$ENDIF}

implementation

{ ECloudApiException }

constructor ECloudApiException.Create(const ACode, ADescription: string; ARequest: IRequestData);
begin
  FCode := ACode;
  FDescription := ADescription;
  FRequest := ARequest;
  FTime := Now;
  inherited Create(FCode + ': ' + Description);
end;

constructor ECloudApiException.Create(const ACode: Integer; const ADescription: string; ARequest: IRequestData);
begin
  Create(ACode.ToString, ADescription, ARequest);
end;

constructor ECloudApiException.Create(AException: Exception);
begin
  Create(0, AException.Message, nil);
end;

function ECloudApiException.ToString: string;
begin
  Result := '[ ' + DateTimeToStr(FTime) + ' ] (' + FCode + ') ' + FDescription;
end;

end.
