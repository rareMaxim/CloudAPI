unit ZonaAPI.Types;

interface

uses
  CloudAPI.Utils.Json,
  System.Generics.Collections;

type
{$REGION 'znBaseResponse'}
  IznBaseResponse = interface
    ['{807F2D3D-AEE6-4B39-B2AE-C4CD28FCACC2}']
    function host: string;
  end;

  TznBaseResponse = class(TBaseJson, IznBaseResponse)
    function host: string;
  end;
{$ENDREGION}

  IznItemSearch = interface
    ['{D55E222A-15D1-4983-B366-8C08D6A81C8C}']
    function cover: string;
    function id: Integer;
    function name_id: string;
    function name_rus: string;
    function serial: Boolean;
    function year: Integer;
  end;

  TznItemSearch = class(TznBaseResponse, IznItemSearch)
    function cover: string;
    function id: Integer;
    function name_id: string;
    function name_rus: string;
    function serial: Boolean;
    function year: Integer;
  end;

  IznItem = interface(IznItemSearch)
    ['{97FC97E3-DA20-41EF-BAB1-3EBB370E8AE6}']
    function mobi_link_id: Integer;
    function name_eng: string;
    function rating: Single;
    function rating_imdb_count: Single;
    function rating_kinopoisk: Single;
    function rating_kinopoisk_count: Integer;
    function serial_ended: Boolean;
    function serial_end_year: Integer;
  end;

  TznItem = class(TznItemSearch, IznItem)
    function mobi_link_id: Integer;
    function name_eng: string;
    function rating: Single;
    function rating_imdb_count: Single;
    function rating_kinopoisk: Single;
    function rating_kinopoisk_count: Integer;
    function serial_ended: Boolean;
    function serial_end_year: Integer;
  end;

  IznPagination = interface
    ['{6625D565-2043-4798-937D-A552865ECD74}']
    function current_page: Integer;
    function limit: Integer;
    function offset: Integer;
    function total_items: Integer;
    function total_pages: Integer;
    function prev_url: string;
    function next_url: string;
  end;

  TznPagination = class(TBaseJson, IznPagination)
    function current_page: Integer;
    function limit: Integer;
    function offset: Integer;
    function total_items: Integer;
    function total_pages: Integer;
    function prev_url: string;
    function next_url: string;
  end;

  IznCategory = interface(IznBaseResponse)
    ['{F948032F-F0BF-4C74-AAA5-232457D3FA53}']
    function items: TArray<IznItem>;
    function pagination: IznPagination;
    function title_h1: string;
  end;

  TznCategory = class(TznBaseResponse, IznCategory)
    function items: TArray<IznItem>;
    function pagination: IznPagination;
    function title_h1: string;
  end;

  IznFilterCountries = interface
    ['{86F100EC-936D-4074-A720-F7BB4745CB9B}']
    function id: Integer;
    function ids: TArray<Integer>;
    function name: string;
    function size: Integer;
    function translit: string;
  end;

  TznFilterCountries = class(TBaseJson, IznFilterCountries)
    function id: Integer;
    function ids: TArray<Integer>;
    function name: string;
    function size: Integer;
    function translit: string;
  end;

  IznFilterGenres = interface
    ['{29BD22D7-F75B-40D0-98DA-A3806746BA06}']
    function adult: Boolean;
    function custom: Boolean;
    function fictional: Boolean;
    function id: Integer;
    function name: string;
    function Ord: Integer;
    function size: Integer;
    function translit: string;
  end;

  TznFilterGenres = class(TBaseJson, IznFilterGenres)
    function adult: Boolean;
    function custom: Boolean;
    function fictional: Boolean;
    function id: Integer;
    function name: string;
    function Ord: Integer;
    function size: Integer;
    function translit: string;
  end;

  IznFilter = interface
    ['{244215B2-0A60-4EE4-BE16-74F079E93A7E}']
    function countries: TArray<IznFilterCountries>;
    function genres: TArray<IznFilterGenres>;
  end;

  TznFilter = class(TBaseJson, IznFilter)
    function countries: TArray<IznFilterCountries>;
    function genres: TArray<IznFilterGenres>;
  end;

  IznTrailer = interface
    ['{EB665F5B-9173-484C-B023-02CC34497943}']
    function url: string;
    function id: string;
  end;

  TznTrailer = class(TBaseJson, IznTrailer)
    function url: string;
    function id: string;
  end;

  IznRuntime = interface
    ['{594F3397-CE3B-4211-9E67-2E02AE815C32}']
    function value: Integer;
    function convert: string;
  end;

  TznRuntime = class(TBaseJson, IznRuntime)
    function value: Integer;
    function convert: string;
  end;

  IznBackdrops = interface
    ['{468C5C87-AE22-4FD0-8ED6-78EBF8B014BF}']
    function image_320: string;
    function image_640: string;
    function image_1000: string;
    function image_1280: string;
    function image_1920: string;
  end;

  TznBackdrops = class(TBaseJson, IznBackdrops)
    function image_320: string;
    function image_640: string;
    function image_1000: string;
    function image_1280: string;
    function image_1920: string;
  end;

  IznSettings = interface
    ['{7D8F3085-6861-49D7-B700-F856BE3D86CF}']
    function quality: string;
    function auto: Boolean;
  end;

  TznSettings = class(TBaseJson, IznSettings)
    function quality: string;
    function auto: Boolean;
  end;

  IznCookies = interface
    ['{F16E8B8A-EAF1-47DF-BBA0-3BF969537D55}']
    function settings: IznSettings;
    function episodes: string;
    function god_mode: string;
  end;

  TznCookies = class(TBaseJson, IznCookies)
    function settings: IznSettings;
    function episodes: string;
    function god_mode: string;
  end;

  IznMediaBase = interface
    ['{AF9FA7C0-C703-4C4E-9406-6E9F1660BF7C}']
    function description: string;
    function trailer: IznTrailer;
    function runtime: IznRuntime;
    function release_date_int: string;
    function release_year_rus: Integer;
    function release_date_rus: string;
    function release_date_hq: string;
    function rating_kinopoisk: Integer;
    function rating_imdb: Single;
    function image: string;
    function rating_imdb_count: Integer;
    function name_original: string;
    function abuse: string;
    function year: Integer;
    function name_id: string;
    function country: string;
    function trailer_url: string;
    function rating_kinopoisk_count: Integer;
    function genreId: string;
    function id: Integer;
    function mobi_link_date: string;
    function country_id: Integer;
    function name_rus: string;
    function persons: string;
    function mobi_link_id: Integer;
    function rating: Single;
    function serial: Boolean;
  end;

  TznMediaBase = class(TBaseJson, IznMediaBase)
    function description: string;
    function trailer: IznTrailer;
    function runtime: IznRuntime;
    function release_date_int: string;
    function release_year_rus: Integer;
    function release_date_rus: string;
    function release_date_hq: string;
    function rating_kinopoisk: Integer;
    function rating_imdb: Single;
    function image: string;
    function rating_imdb_count: Integer;
    function name_original: string;
    function abuse: string;
    function year: Integer;
    function name_id: string;
    function country: string;
    function trailer_url: string;
    function rating_kinopoisk_count: Integer;
    function genreId: string;
    function id: Integer;
    function mobi_link_date: string;
    function country_id: Integer;
    function name_rus: string;
    function persons: string;
    function mobi_link_id: Integer;
    function rating: Single;
    function serial: Boolean;
  end;

  IznSerial = interface(IznMediaBase)
    ['{68957204-CA9B-41CB-8412-CDD11CE5F8E4}']
    function backdrop_id: Integer;
  end;

  TznSerial = class(TznMediaBase, IznSerial)
    function backdrop_id: Integer;
  end;

  IznMovie = interface(IznMediaBase)
    ['{9D89E139-316C-4D33-A6B2-F2996D2BD3DE}']
  end;

  TznMovie = class(TznMediaBase, IznMovie);

  IznSeo = interface
    ['{1601F697-3B89-4B2D-AA73-48B8F738116D}']
    function seo_title: string;
    function seo_keywords: string;
    function seo_h1: string;
    function seo_h2: string;
    function seo_footer: string;
  end;

  TznSeo = class(TBaseJson, IznSeo)
    function seo_title: string;
    function seo_keywords: string;
    function seo_h1: string;
    function seo_h2: string;
    function seo_footer: string;
  end;

  IznEpisode = interface
    ['{455AF9CA-2AEE-4CA7-8DFE-ECC746118B49}']
    function episode: Integer;
    function episode_key: string;
    function mobi_link_id: Integer;
    function release_date: string;
    function season: Integer;
    function title: string;
  end;

  TznEpisode = class(TBaseJson, IznEpisode)
    function episode: Integer;
    function episode_key: string;
    function mobi_link_id: Integer;
    function release_date: string;
    function season: Integer;
    function title: string;
  end;

  IznEpisodes = interface
    ['{CA93172E-7C8B-4020-B9D8-314BC04843FD}']
    function count: Integer;
    function count_all: Integer;
    function current: Integer;
    function current_mobi_link_id: Integer;
    function items: TArray<IznEpisode>;
  end;

  TznEpisodes = class(TBaseJson, IznEpisodes)
    function count: Integer;
    function count_all: Integer;
    function current: Integer;
    function current_mobi_link_id: Integer;
    function items: TArray<IznEpisode>;
  end;

  IznSeasons = interface
    ['{3FA6CB3F-4370-4D1F-88F4-698FB1FE3CF5}']
    function count: Integer;
    function current: Integer;
  end;

  TznSeasons = class(TBaseJson, IznSeasons)
    function count: Integer;
    function current: Integer;
  end;

  IznPageInfo = interface(IznBaseResponse)
    ['{7BFEDD62-62DF-4473-83F9-9FB1623C19CA}']
    function movie: IznMovie;
    function serial: IznSerial;
    function cookies: IznCookies;
    function persons: string;
    function genres: TArray<IznFilterGenres>;
    function countries: TArray<IznFilterCountries>;
    function video_frames: TArray<string>;
    function backdrops: IznBackdrops;
    function is_mobile: Boolean;
    function redirect: string;
    function seo: IznSeo;
    function episodes: IznEpisodes;
    function seasons: IznSeasons;
  end;

  TznPageInfo = class(TznBaseResponse, IznPageInfo)
    function movie: IznMovie;
    function serial: IznSerial;
    function cookies: IznCookies;
    function persons: string;
    function genres: TArray<IznFilterGenres>;
    function countries: TArray<IznFilterCountries>;
    function video_frames: TArray<string>;
    function backdrops: IznBackdrops;
    function is_mobile: Boolean;
    function redirect: string;
    function seo: IznSeo;
    function episodes: IznEpisodes;
    function seasons: IznSeasons;
  end;

  IznVideoLink = interface(IznBaseResponse)
    ['{E4801788-6CFB-4F2A-8737-72EE015DF95E}']
    function images: TArray<string>;
    function Url: string;
  end;

  TznVideoLink = class(TznBaseResponse, IznVideoLink)
    function images: TArray<string>;
    function Url: string;
  end;

  IznSuggest = interface(IznBaseResponse)
    ['{81A5A2FB-3BE4-4DAE-94FD-E792762B0D66}']
    function items: TArray<IznItemSearch>;
  end;

  TznSuggest = class(TznBaseResponse, IznSuggest)
    function items: TArray<IznItemSearch>;
  end;

  IznQuery = interface
    ['{B4F37BAF-620A-47E7-A023-4B2B335D223D}']
    function current: string;
    function second: string;
  end;

  TznQuery = class(TBaseJson, IznQuery)
    function current: string;
    function second: string;
  end;

  IznSearch = interface(IznCategory)
    ['{1A09CA3C-9A4A-4A01-A68E-FAA2A6034245}']
    function is_second: Boolean;
    function query: IznQuery;
    function search_title: string;
    function num: Integer;
  end;

  TznSearch = class(TznCategory, IznSearch)
    function is_second: Boolean;
    function query: IznQuery;
    function search_title: string;
    function num: Integer;
  end;

implementation

{ TznCategory }

function TznCategory.items: TArray<IznItem>;
begin
  Result := ToArray<IznItem>(TznItem, 'items');
end;

function TznCategory.pagination: IznPagination;
begin
  Result := ToClass<TznPagination>('pagination');
end;

function TznCategory.title_h1: string;
begin
  Result := ToSimpleType<string>('title_h1');
end;

{ TznItemSearch }

function TznItemSearch.cover: string;
begin
  Result := ToSimpleType<string>('cover');
end;

function TznItemSearch.id: Integer;
begin
  Result := ToSimpleType<Integer>('id');
end;

function TznItemSearch.name_id: string;
begin
  Result := ToSimpleType<string>('name_id');
end;

function TznItemSearch.name_rus: string;
begin
  Result := ToSimpleType<string>('name_rus');
end;

function TznItemSearch.serial: Boolean;
begin
  Result := ToSimpleType<Boolean>('serial');
end;

function TznItemSearch.year: Integer;
begin
  Result := ToSimpleType<Integer>('year');
end;
{ TznItem }

function TznItem.mobi_link_id: Integer;
begin
  Result := ToSimpleType<Integer>('mobi_link_id');
end;

function TznItem.name_eng: string;
begin
  Result := ToSimpleType<string>('name_eng');
end;

function TznItem.rating: Single;
begin
  Result := ToSimpleType<Single>('rating');
end;

function TznItem.rating_imdb_count: Single;
begin
  Result := ToSimpleType<Single>('rating_imdb_count');
end;

function TznItem.rating_kinopoisk: Single;
begin
  Result := ToSimpleType<Single>('rating_kinopoisk');
end;

function TznItem.rating_kinopoisk_count: Integer;
begin
  Result := ToSimpleType<Integer>('rating_kinopoisk_count');
end;

function TznItem.serial_ended: Boolean;
begin
  Result := ToSimpleType<Boolean>('serial_ended');
end;

function TznItem.serial_end_year: Integer;
begin
  Result := ToSimpleType<Integer>('serial_end_year');
end;



{ TznPagination }

function TznPagination.current_page: Integer;
begin
  Result := ToSimpleType<Integer>('current_page');
end;

function TznPagination.limit: Integer;
begin
  Result := ToSimpleType<Integer>('limit');
end;

function TznPagination.next_url: string;
begin
  Result := ToSimpleType<string>('next_url');
end;

function TznPagination.offset: Integer;
begin
  Result := ToSimpleType<Integer>('offset');
end;

function TznPagination.prev_url: string;
begin
  Result := ToSimpleType<string>('prev_url');
end;

function TznPagination.total_items: Integer;
begin
  Result := ToSimpleType<Integer>('total_items');
end;

function TznPagination.total_pages: Integer;
begin
  Result := ToSimpleType<Integer>('total_pages');
end;

{ TznFilterCountries }

function TznFilterCountries.id: Integer;
begin
  Result := ToSimpleType<Integer>('id');
end;

function TznFilterCountries.ids: TArray<Integer>;
begin
  Result := ToSimpleArray<Integer>('total_pages');
end;

function TznFilterCountries.name: string;
begin
  Result := ToSimpleType<string>('name');
end;

function TznFilterCountries.size: Integer;
begin
  Result := ToSimpleType<Integer>('size');
end;

function TznFilterCountries.translit: string;
begin
  Result := ToSimpleType<string>('translit');
end;


{ TznFilterGenres }

function TznFilterGenres.adult: Boolean;
begin
  Result := ToSimpleType<Boolean>('adult');
end;

function TznFilterGenres.custom: Boolean;
begin
  Result := ToSimpleType<Boolean>('custom');
end;

function TznFilterGenres.fictional: Boolean;
begin
  Result := ToSimpleType<Boolean>('fictional');
end;

function TznFilterGenres.id: Integer;
begin
  Result := ToSimpleType<Integer>('id');
end;

function TznFilterGenres.name: string;
begin
  Result := ToSimpleType<string>('name');
end;

function TznFilterGenres.Ord: Integer;
begin
  Result := ToSimpleType<Integer>('Ord');
end;

function TznFilterGenres.size: Integer;
begin
  Result := ToSimpleType<Integer>('size');
end;

function TznFilterGenres.translit: string;
begin
  Result := ToSimpleType<string>('translit');
end;

{ TznFilter }

function TznFilter.countries: TArray<IznFilterCountries>;
begin
  Result := ToArray<IznFilterCountries>(TznFilterCountries, 'countries');
end;

function TznFilter.genres: TArray<IznFilterGenres>;
begin
  TBaseJson.UnSupported;
 // Result := ReadToArray<IznFilterGenres>(TznFilterGenres, 'genres');
end;

{ TznBaseResponse }

function TznBaseResponse.host: string;
begin
  Result := ToSimpleType<string>('host');
end;

{ TznTrailer }

function TznTrailer.id: string;
begin
  Result := ToSimpleType<string>('id');
end;

function TznTrailer.url: string;
begin
  Result := ToSimpleType<string>('url');
end;

{ TznRuntime }

function TznRuntime.convert: string;
begin
  Result := ToSimpleType<string>('convert');
end;

function TznRuntime.value: Integer;
begin
  Result := ToSimpleType<Integer>('value');
end;

{ TznBackdrops }

function TznBackdrops.image_1000: string;
begin
  Result := ToSimpleType<string>('image_1000');
end;

function TznBackdrops.image_1280: string;
begin
  Result := ToSimpleType<string>('image_1280');
end;

function TznBackdrops.image_1920: string;
begin
  Result := ToSimpleType<string>('image_1920');
end;

function TznBackdrops.image_320: string;
begin
  Result := ToSimpleType<string>('image_320');
end;

function TznBackdrops.image_640: string;
begin
  Result := ToSimpleType<string>('image_640');
end;

{ TznSettings }

function TznSettings.auto: Boolean;
begin
  Result := ToSimpleType<Boolean>('auto');
end;

function TznSettings.quality: string;
begin
  Result := ToSimpleType<string>('quality');
end;

{ TznCookies }

function TznCookies.episodes: string;
begin
  Result := ToSimpleType<string>('episodes');
end;

function TznCookies.god_mode: string;
begin
  Result := ToSimpleType<string>('god_mode');
end;

function TznCookies.settings: IznSettings;
begin
  Result := ToClass<TznSettings>('settings');
end;

{ TznMediaBase }

function TznMediaBase.abuse: string;
begin
  Result := ToSimpleType<string>('abuse');
end;

function TznMediaBase.country: string;
begin
  Result := ToSimpleType<string>('country');
end;

function TznMediaBase.country_id: Integer;
begin
  Result := ToSimpleType<Integer>('country_id');
end;

function TznMediaBase.description: string;
begin
  Result := ToSimpleType<string>('description');
end;

function TznMediaBase.genreId: string;
begin
  Result := ToSimpleType<string>('genreId');
end;

function TznMediaBase.id: Integer;
begin
  Result := ToSimpleType<Integer>('id');
end;

function TznMediaBase.image: string;
begin
  Result := ToSimpleType<string>('image');
end;

function TznMediaBase.mobi_link_date: string;
begin
  Result := ToSimpleType<string>('mobi_link_date');
end;

function TznMediaBase.mobi_link_id: Integer;
begin
  Result := ToSimpleType<Integer>('mobi_link_id');
end;

function TznMediaBase.name_id: string;
begin
  Result := ToSimpleType<string>('name_id');
end;

function TznMediaBase.name_original: string;
begin
  Result := ToSimpleType<string>('name_original');
end;

function TznMediaBase.name_rus: string;
begin
  Result := ToSimpleType<string>('name_rus');
end;

function TznMediaBase.persons: string;
begin
  Result := ToSimpleType<string>('persons');
end;

function TznMediaBase.rating: Single;
begin
  Result := ToSimpleType<Single>('rating');
end;

function TznMediaBase.rating_imdb: Single;
begin
  Result := ToSimpleType<Single>('rating_imdb');
end;

function TznMediaBase.rating_imdb_count: Integer;
begin
  Result := ToSimpleType<Integer>('rating_imdb_count');
end;

function TznMediaBase.rating_kinopoisk: Integer;
begin
  Result := ToSimpleType<Integer>('rating_kinopoisk');
end;

function TznMediaBase.rating_kinopoisk_count: Integer;
begin
  Result := ToSimpleType<Integer>('rating_kinopoisk_count');
end;

function TznMediaBase.release_date_hq: string;
begin
  Result := ToSimpleType<string>('release_date_hq');
end;

function TznMediaBase.release_date_int: string;
begin
  Result := ToSimpleType<string>('release_date_int');
end;

function TznMediaBase.release_date_rus: string;
begin
  Result := ToSimpleType<string>('release_date_rus');
end;

function TznMediaBase.release_year_rus: Integer;
begin
  Result := ToSimpleType<Integer>('release_year_rus');
end;

function TznMediaBase.runtime: IznRuntime;
begin
  Result := ToClass<TznRuntime>('runtime');
end;

function TznMediaBase.serial: Boolean;
begin
  Result := ToSimpleType<Boolean>('serial');
end;

function TznMediaBase.trailer: IznTrailer;
begin
  Result := ToClass<TznTrailer>('trailer');
end;

function TznMediaBase.trailer_url: string;
begin
  Result := ToSimpleType<string>('trailer_url');
end;

function TznMediaBase.year: Integer;
begin
  Result := ToSimpleType<Integer>('year');
end;

{ TznSerial }

function TznSerial.backdrop_id: Integer;
begin
  Result := ToSimpleType<Integer>('backdrop_id');
end;

{ TznSeo }

function TznSeo.seo_footer: string;
begin
  Result := ToSimpleType<string>('seo_footer')
end;

function TznSeo.seo_h1: string;
begin
  Result := ToSimpleType<string>('seo_h1')
end;

function TznSeo.seo_h2: string;
begin
  Result := ToSimpleType<string>('seo_h2')
end;

function TznSeo.seo_keywords: string;
begin
  Result := ToSimpleType<string>('seo_keywords')

end;

function TznSeo.seo_title: string;
begin
  Result := ToSimpleType<string>('seo_title')
end;

{ TznPageInfo }

function TznPageInfo.backdrops: IznBackdrops;
begin
  Result := ToClass<TznBackdrops>('backdrops');
end;

function TznPageInfo.cookies: IznCookies;
begin
  Result := ToClass<TznCookies>('cookies');
end;

function TznPageInfo.countries: TArray<IznFilterCountries>;
begin
  Result := ToArray<IznFilterCountries>(TznFilterCountries, 'countries');
end;

function TznPageInfo.episodes: IznEpisodes;
begin
  Result := ToClass<TznEpisodes>('episodes');
end;

function TznPageInfo.genres: TArray<IznFilterGenres>;
begin
  Result := ToArray<IznFilterGenres>(TznFilterGenres, 'genres');
end;

function TznPageInfo.is_mobile: Boolean;
begin
  Result := ToSimpleType<Boolean>('is_mobile');
end;

function TznPageInfo.movie: IznMovie;
begin
  Result := ToClass<TznMovie>('movie');
end;

function TznPageInfo.persons: string;
begin
  Result := ToSimpleType<string>('persons');
end;

function TznPageInfo.redirect: string;
begin
  Result := ToSimpleType<string>('redirect');
end;

function TznPageInfo.seasons: IznSeasons;
begin
  Result := ToClass<TznSeasons>('seasons');
end;

function TznPageInfo.seo: IznSeo;
begin
  Result := ToClass<TznSeo>('seo');
end;

function TznPageInfo.serial: IznSerial;
begin
  Result := ToClass<TznSerial>('serial');
end;

function TznPageInfo.video_frames: TArray<string>;
begin
  Result := ToSimpleArray<string>('video_frames');
end;

{ TznVideoLink }

function TznVideoLink.images: TArray<string>;
begin
  Result := ToSimpleArray<string>('images');
end;

function TznVideoLink.url: string;
begin
  Result := ToSimpleType<string>('url');
end;

{ TznSuggest }

function TznSuggest.items: TArray<IznItemSearch>;
begin
  Result := ToArray<IznItemSearch>(TznItemSearch, 'items');
end;

{ TznSearch }

function TznSearch.is_second: Boolean;
begin
  Result := ToSimpleType<Boolean>('is_second');
end;

function TznSearch.num: Integer;
begin
  Result := ToSimpleType<Integer>('num');
end;

function TznSearch.query: IznQuery;
begin
  Result := ToClass<TznQuery>('query');
end;

function TznSearch.search_title: string;
begin
  Result := ToSimpleType<string>('search_title');
end;

{ TznQuery }

function TznQuery.current: string;
begin
  Result := ToSimpleType<string>('current');
end;

function TznQuery.second: string;
begin
  Result := ToSimpleType<string>('second');
end;

{ TznEpisode }

function TznEpisode.episode: Integer;
begin
  Result := ToSimpleType<Integer>('episode');
end;

function TznEpisode.episode_key: string;
begin
  Result := ToSimpleType<string>('episode_key');
end;

function TznEpisode.mobi_link_id: Integer;
begin
  Result := ToSimpleType<Integer>('mobi_link_id');

end;

function TznEpisode.release_date: string;
begin
  Result := ToSimpleType<string>('release_date');
end;

function TznEpisode.season: Integer;
begin
  Result := ToSimpleType<Integer>('season');
end;

function TznEpisode.title: string;
begin
  Result := ToSimpleType<string>('title');
end;

{ TznEpisodes }

function TznEpisodes.count: Integer;
begin
  Result := ToSimpleType<Integer>('count');
end;

function TznEpisodes.count_all: Integer;
begin
  Result := ToSimpleType<Integer>('count_all');
end;

function TznEpisodes.current: Integer;
begin
  Result := ToSimpleType<Integer>('current');
end;

function TznEpisodes.current_mobi_link_id: Integer;
begin
  Result := ToSimpleType<Integer>('current_mobi_link_id');
end;

function TznEpisodes.items: TArray<IznEpisode>;
begin
  Result := ToPairsAsArray<IznEpisode>(TznEpisode, 'items');
end;

{ TznSeasons }

function TznSeasons.count: Integer;
begin
  Result := ToSimpleType<Integer>('count');

end;

function TznSeasons.current: Integer;
begin
  Result := ToSimpleType<Integer>('current');
end;

end.

