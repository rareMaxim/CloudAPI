unit ZonaAPI;

interface

uses
  ZonaAPI.Types,
  ZonaAPI.FilterProcessor,
  CloudAPI.Request,
  System.SysUtils,
  System.Classes;

type
  TZonaAPI = class(TComponent)
  private
    FFilter: TznFilterProcessor;
    FTempHost: string;
    function GetAPI: IApiRequest;
  protected
    function GetMedia(const AType: string; const APage: Integer = 1): IznCategory;
  public
    function GetSeries(const APage: Integer = 1): IznCategory;
    function GetMovies(const APage: Integer = 1): IznCategory;
    function GetFilters: IznFilter;
    function GetPageInfo(AItem: IznItem): IznPageInfo;
    function GetVideoLink(APageInfo: IznPageInfo): IznVideoLink;
    function Suggest(const ASearch: string): TArray<IznItemSearch>;
    function Search(const ARequest: string; const APage: Integer = 1): IznSearch;
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
    property Filter: TznFilterProcessor read FFilter write FFilter;
  end;

implementation

{ TZonaAPI }

constructor TZonaAPI.Create(AOwner: TComponent);
begin
  inherited;
  FFilter := TznFilterProcessor.Create;
  FTempHost := 'w1.zona.plus/';
end;

destructor TZonaAPI.Destroy;
begin
  FFilter.Free;
  inherited;
end;

function TZonaAPI.GetAPI: IApiRequest;
begin
  Result := TApiRequest.Create;
  Result.Url := 'https://' + FTempHost;
  Result.AddParameter('Accept', 'application/json, text/javascript, */*; q=0.01',
    '', True, TStoreFormat.InHeader);
  Result.AddParameter('Upgrade-Insecure-Requests', '1', '', True, TStoreFormat.InHeader);
  Result.AddParameter('X-Requested-With', 'XMLHttpRequest', '', True,
    TStoreFormat.InHeader);
  Result.AddParameter('UserAgent',
    'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.91 Safari/537.36 Vivaldi/1.93.955.36',
    '', True, TStoreFormat.InHeader);
end;

function TZonaAPI.GetFilters: IznFilter;
begin
  Result := TznFilter.Create(GetAPI.SetMethod('/ajax/widget/filter').ExecuteAsString);
end;

function TZonaAPI.GetMedia(const AType: string; const APage: Integer): IznCategory;
var
  LApi: IApiRequest;
begin
  LApi := GetAPI.SetMethod('/' + AType + '/');
  if APage > 1 then
    LApi.AddParameter('page', APage, 1, False, TStoreFormat.InUrl);
  Result := TznCategory.Create(LApi.ExecuteAsString);
end;

function TZonaAPI.GetSeries(const APage: Integer): IznCategory;
begin
  Result := GetMedia('tvseries', APage);
end;

function TZonaAPI.GetMovies(const APage: Integer): IznCategory;
begin
  Result := GetMedia('movies', APage);
end;

function TZonaAPI.GetPageInfo(AItem: IznItem): IznPageInfo;
var
  LMethod: string;
begin
  if AItem.serial then
    LMethod := 'tvseries/' + AItem.name_id
  else
    LMethod := 'movies/' + AItem.name_id;
  Result := TznPageInfo.Create(GetAPI.SetMethod(LMethod).ExecuteAsString);
end;

function TZonaAPI.GetVideoLink(APageInfo: IznPageInfo): IznVideoLink;
var
  LID: string;
begin
  if APageInfo.serial = nil then
    LID := APageInfo.movie.mobi_link_id.ToString
  else
    LID := APageInfo.serial.mobi_link_id.ToString;
  Result := TznVideoLink.Create(GetAPI.SetMethod('/ajax/video/' + LID).ExecuteAsString);
end;

function TZonaAPI.Search(const ARequest: string; const APage: Integer): IznSearch;
var
  LApi: IApiRequest;
begin
  LApi := GetAPI.SetMethod('/search/' + ARequest);
  if APage > 1 then
    LApi.AddParameter('page', APage, 1, False, TStoreFormat.InUrl);
  Result := TznSearch.Create(LApi.ExecuteAsString);
  FTempHost := Result.host;
end;

function TZonaAPI.Suggest(const ASearch: string): TArray<IznItemSearch>;
var
  LSugg: IznSuggest;
begin
  LSugg := TznSuggest.Create(GetAPI.SetMethod('/ajax/suggest/' + ASearch).ExecuteAsString);
  FTempHost := LSugg.host;
  Result := LSugg.items;
end;

end.

