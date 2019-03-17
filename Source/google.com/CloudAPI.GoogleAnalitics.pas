unit CloudAPI.GoogleAnalitics;

interface

uses
  CloudAPI.BaseComponent,
  CloudAPI.GoogleAnalitics.Extensions,
  System.Classes,
  System.SysUtils;

type
  /// <summary>
  /// Класс для работы с Analitics.Google.com: Measurement Protocol.
  /// </summary>

  TgaAnalitics = class(TCloudApiBaseComponent)
  private
    const
      SRV = 'https://www.google-analytics.com';
  private
    FGeneral: TgaGeneral;
    FUser: TgaUser;
    FSystem: TgaSystem;
    FSessionController: TgaSession;
    FContentInformation: TgaContentInformation;
    FException: TgaException;
    FHit: TgaHit;
    FTrafficSources: TgaTrafficSources;
    FAppTracking: TgaAppTracking;
  protected
  
    function Execute(const AUrl, AType: string): TBytes;
    procedure DoExecute(const AType: string);
    procedure DoInitApiCore; override;
  public
    procedure pageview(const APage: string);
    procedure HaveException(const AText: string; AIsFatal: Boolean = True);
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
  published
    property General: TgaGeneral read FGeneral write FGeneral;
    property User: TgaUser read FUser write FUser;
    property Session: TgaSession read FSessionController write FSessionController;
    property TrafficSources: TgaTrafficSources read FTrafficSources write FTrafficSources;
    property SystemInfo: TgaSystem read FSystem write FSystem;
    property Hit: TgaHit read FHit write FHit;
    property ContentInformation: TgaContentInformation read FContentInformation
      write FContentInformation;
    property AppTracking: TgaAppTracking read FAppTracking write FAppTracking;
    property Exception: TgaException read FException write FException;
    property OnSendData;
  end;
{$SCOPEDENUMS ON}

procedure register;

implementation

uses
  CloudAPI.Request;

procedure register;
begin
  RegisterComponents('CloudAPI', [TgaAnalitics]);
end;

  { TgaAnalitics }
constructor TgaAnalitics.Create(AOwner: TComponent);
begin
  inherited;
  FGeneral := TgaGeneral.Create;
  FUser := TgaUser.Create;
  FSystem := TgaSystem.Create;
  FTrafficSources := TgaTrafficSources.Create;
  FHit := TgaHit.Create;
  FSessionController := TgaSession.Create;
  FContentInformation := TgaContentInformation.Create;
  FAppTracking := TgaAppTracking.Create;
  FException := TgaException.Create;
end;

destructor TgaAnalitics.Destroy;
begin
  FreeAndNil(FGeneral);
  FreeAndNil(FUser);
  FreeAndNil(FSystem);
  FreeAndNil(FTrafficSources);
  FreeAndNil(FHit);
  FreeAndNil(FSessionController);
  FreeAndNil(FContentInformation);
  FreeAndNil(FAppTracking);
  FreeAndNil(FException);
  inherited;
end;

procedure TgaAnalitics.DoExecute(const AType: string);
begin
  Writeln(TEncoding.Default.GetString(Execute(SRV, AType)));
end;

procedure TgaAnalitics.DoInitApiCore;
begin
  inherited;

end;

procedure TgaAnalitics.HaveException(const AText: string; AIsFatal: Boolean);
begin
  Exception.Text := AText;
  Exception.IsFatal := AIsFatal;
  DoExecute('exception')
end;

function TgaAnalitics.Execute(const AUrl, AType: string): TBytes;
begin
  User.FillData(AType, GetRequest.StoreUrl);
  SystemInfo.FillData(AType, GetRequest.StoreUrl);
  FHit.FillData(AType, GetRequest.StoreUrl);
  Session.FillData(AType, GetRequest.StoreUrl);
  ContentInformation.FillData(AType, GetRequest.StoreUrl);
  Exception.FillData(AType, GetRequest.StoreUrl);
  General.FillData(AType, GetRequest.StoreUrl);
  GetRequest.Domain := AUrl;
  GetRequest.MethodUrl:='collect';
  Result := GetRequest.ExecuteAsBytes;
end;

procedure TgaAnalitics.pageview(const APage: string);
begin
  ContentInformation.DocumentPath := APage;
  DoExecute('pageview');
end;

end.

