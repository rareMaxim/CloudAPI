unit ZonaRu;

interface

uses
  System.Classes,
  CloudAPI.BaseComponent,
  ZonaRu.Query,
  ZonaRu.Types;

type
  TZona = class(TCloudApiBaseComponent)
  private
    FQuery: TZonaQuery;
  protected
    function ExractDocs(const AInput: string): string;
    procedure DoInitApiCore; override;
  protected
    function GetMedia: TArray<IznCoverSerial>;
  public

    function GetMovies: TArray<IznCoverSerial>;
    function GetSerials: TArray<IznCoverSerial>;
    function OpenMedia(const ID: Integer): IznItemFull;
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  CloudAPI.Request,
  CloudAPI.Utils.JSON,
  System.SysUtils,
  System.JSON;
{ TZona }

constructor TZona.Create(AOwner: TComponent);
begin
  inherited;
  FQuery := TZonaQuery.Create;
end;

destructor TZona.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TZona.DoInitApiCore;
begin
  inherited;
  Domain := 'http://zsolr3.zonasearch.com/solr';
  GetRequest.StoreAutoFormat := TStoreFormat.InUrl;
  GetRequest.OnStaticFill := procedure
    begin
      with GetRequest do
      begin
        AddParameter('Accept', 'text/html, image/gif, image/jpeg, *; q=.2, */*; q=.2', '', True, TStoreFormat.InHeader);
        AddParameter('Accept-Encoding', 'gzip, deflate', '', True, TStoreFormat.InHeader);
        AddParameter('Connection', 'keep-alive', '', True, TStoreFormat.InHeader);
        AddParameter('User-Agent', 'Java/1.8.0_171', '', True, TStoreFormat.InHeader);
      end;

    end;
  GetRequest.OnDataReceiveAsString := function(AInput: string): string
    var
      LJSON: TJSONObject;
    begin
      if Assigned(OnReceiveRawData) then
        OnReceiveRawData(Self, AInput);
      Result := '';
      if AInput.IsEmpty or AInput.StartsWith('<html') then
        Exit;
      LJSON := TJSONObject.ParseJSONValue(AInput) as TJSONObject;
      try
        Result := LJSON.GetValue('response').ToString;
      finally
        LJSON.Free;
      end;
    end;
  GetRequest.OnDataSend := procedure(AUrl, AData, AHeaders: string)
    begin
      if Assigned(OnSendData) then
        OnSendData(Self, AUrl, AData);
    end;
end;

function TZona.ExractDocs(const AInput: string): string;
var
  LJSON: TJSONObject;
begin
  Result := '';
  LJSON := TJSONObject.ParseJSONValue(AInput) as TJSONObject;
  try
    Result := LJSON.GetValue('docs').ToString;
  finally
    LJSON.Free;
  end;
end;

function TZona.GetMedia: TArray<IznCoverSerial>;
const
  // CQ = '(NOT(abuse:zona)AND(adult:false)AND(tor_count:[1+TO+2147483647])AND(indexed:[1+TO+7])AND(serial:false)NOT(genreId:(12+OR+15+OR+25+OR+26+OR+1747+OR+28+OR+27+OR+tv)))';
  CFL1 = 'id,year,playable,trailer,quality,audio_quality,type3d,serial,languages_imdb,rating,genre,runtime,episodes,tor_count,serial_end_year,serial_ended,abuse,';
  CFL2 = 'release_date_int,release_date_rus,indexed,geo_rules,partner_entity_id,partner_type,name_rus,name_ukr,name_eng,name_original';
  CFL = CFL1 + CFL2;
var
  LResp: String;
begin
  with GetRequest do
  begin
    SetMethod('movie/select');
    AddParameter('q', FQuery.ToCatalog, '', True);
    AddParameter('version', '2.2', '', True);
    AddParameter('wt', 'json', '', True);
    AddParameter('sort', 'popularity desc,seeds desc,id desc', '', True);
    AddParameter('fl', CFL, '', True);
    AddParameter('start', 0, -1, True);
    AddParameter('rows', 60, -1, True);
    LResp := (ExractDocs(ExecuteAsString));
    Result := TBaseJson.AsArray<IznCoverSerial>(TznCoverSerial, LResp);
  end;
end;

function TZona.GetMovies: TArray<IznCoverSerial>;
begin
  FQuery.Serial := False;
  Result := GetMedia;
end;

function TZona.GetSerials: TArray<IznCoverSerial>;
begin
  FQuery.Serial := True;
  Result := GetMedia;
end;

function TZona.OpenMedia(const ID: Integer): IznItemFull;
var
  LResp: String;
begin
  FQuery.ID := ID;
  with GetRequest do
  begin
    SetMethod('movie/select');
    AddParameter('q', FQuery.ToItem, '', True);
    AddParameter('version', '2.2', '', True);
    AddParameter('wt', 'json', '', True);
    AddParameter('start', 0, -1, True);
    AddParameter('rows', 60, -1, True);
    LResp := (ExractDocs(ExecuteAsString));
    Result := TBaseJson.AsArray<IznItemFull>(TznItemFull, LResp)[0];
  end;
end;

end.
