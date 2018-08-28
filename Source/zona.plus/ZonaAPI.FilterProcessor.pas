unit ZonaAPI.FilterProcessor;

interface

uses
  System.Classes;

type
  TznFilterProcessor = class
  private
    FGenres: TStringList;
    FYears: TStringList;
    FCountrys: TStringList;
    FRatings: TStringList;
    FSorts: TStringList;
    FGenre: string;
    FYear: string;
    FCountry: string;
    FSort: string;
    FCategoria: string;
    FRating: string;
  protected
    procedure RegisterGenres;
    procedure RegisterAll;
  public
    constructor Create;
    destructor Destroy; override;
    function ToString: string; override;
    property Genre: string read FGenre write FGenre;
    property Genres: TStringList read FGenres write FGenres;
    property Year: string read FYear write FYear;
    property Country: string read FCountry write FCountry;
    property Rating: string read FRating write FRating;
    property Sort: string read FSort write FSort;
    property Categoria: string read FCategoria write FCategoria;
  end;

implementation

uses
  System.Net.URLClient;
{ TFilterProcessor }

constructor TznFilterProcessor.Create;
begin
  FGenres := TStringList.Create;
  FYears := TStringList.Create;
  FCountrys := TStringList.Create;
  FRatings := TStringList.Create;
  FSorts := TStringList.Create;
  RegisterAll;
end;

destructor TznFilterProcessor.Destroy;
begin
  FGenres.Free;
  FYears.Free;
  FCountrys.Free;
  FRatings.Free;
  FSorts.Free;
  inherited;
end;

procedure TznFilterProcessor.RegisterAll;
begin
  RegisterGenres;
end;

procedure TznFilterProcessor.RegisterGenres;
begin
  FGenres //
.AddPair('Драма', 'genre-drama') //
.AddPair('Комедия', 'genre-komediia') //
.AddPair('триллер', 'genre-triller') //
.AddPair('мелодрама', 'genre-melodrama') //
.AddPair('боевик', 'genre-boevik') //
.AddPair('криминал', 'genre-kriminal') //
.AddPair('ужасы', 'genre-uzhasy') //
.AddPair('приключения', 'genre-prikliucheniia') //
.AddPair('фантастика', 'genre-fantastika') //
.AddPair('детектив', 'genre-detektiv') //
.AddPair('фэнтези', 'genre-fentezi') //
.AddPair('семейный', 'genre-semeinyi') //
.AddPair('военный', 'genre-voennyi') //
.AddPair('мультфильм', 'genre-multfilm') //
.AddPair('история', 'genre-istoriia') //
.AddPair('биография', 'genre-biografiia') //
.AddPair('мюзикл', 'genre-miuzikl') //
.AddPair('вестерн', 'genre-vestern') //
.AddPair('музыка', 'genre-muzyka') //
.AddPair('спорт', 'genre-sport') //
.AddPair('документальный', 'genre-dokumentalnyi') //
.AddPair('аниме', 'genre-anime') //
.AddPair('короткометражка', 'genre-korotkometrazhka') //
.AddPair('фильм-нуар', 'genre-film-nuar') //
.AddPair('детский', 'genre-detskii') //
.AddPair('реальное ТВ', 'genre-realnoe-tv') //
.AddPair('новости', 'genre-novosti') //
.AddPair('концерт', 'genre-kontcert') //
.AddPair('ток-шоу', 'genre-tok-shou') //
  // .AddPair('', '') //
  // .AddPair('', '') //
  // .AddPair('', '') //
  // .AddPair('', '') //
  // .AddPair('', '') //
  // .AddPair('', '') //
  // .AddPair('', '') //
end;

function TznFilterProcessor.ToString: string;
var
  LUri: TURI;
begin
  LUri := TURI.Create('https://zona.mobi');
  LUri.Path := LUri.Path + Categoria + '/';
  if FGenres.IndexOfName(Genre) > -1 then
    LUri.Path := LUri.Path + FGenres.Values[Genre] + '/';
  Result := LUri.ToString;
end;

end.

