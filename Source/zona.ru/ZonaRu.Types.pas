unit ZonaRu.Types;

interface

uses
  CloudAPI.Utils.Json;

type

  IznCoverMedia = interface
  ['{2DF78C63-B657-41DE-99FC-173EB5AEB3CB}']
    function type3d: integer;
    function year: integer;
    function release_date_int: string;
    function name_original: string;
    function rating: single;
    function playable: boolean;
    function name_eng: string;
    function id: string;
    function episodes: string;
    function release_date_rus: string;
    function indexed: integer;
    function serial_end_year: integer;
    function runtime: integer;
    function quality: integer;
    function serial_ended: boolean;
    function audio_quality: integer;
    function abuse: string;
    function tor_count: integer;
    function trailer: boolean;
    function genre: string;
    function languages_imdb: string;
    function name_rus: string;
    function serial: boolean;
  end;

  TznCoverMedia = class(TBaseJson, IznCoverMedia)
  public
    function type3d: integer;
    function year: integer;
    function release_date_int: string;
    function name_original: string;
    function rating: single;
    function playable: boolean;
    function name_eng: string;
    function id: string;
    function episodes: string;
    function release_date_rus: string;
    function indexed: integer;
    function serial_end_year: integer;
    function runtime: integer;
    function quality: integer;
    function serial_ended: boolean;
    function audio_quality: integer;
    function abuse: string;
    function tor_count: integer;
    function trailer: boolean;
    function genre: string;
    function languages_imdb: string;
    function name_rus: string;
    function serial: boolean;
  end;

  IznItemFull = interface(IznCoverMedia)
    ['{A80FA048-ED6D-49CA-A782-9A2D4C549247}']
    function _version_: int64;
    function adult: boolean;
    function await_count: integer;
    function await_percent: integer;
    function backdrop_id: integer;
    function budget: integer;
    function color_rgb: integer;
    function country: string;
    function country_id: string;
    function description: string;
    function description_eng: string;
    function episodes: string;
    function episodes_linked: string;
    function genre: string;
    function genreId: string;
    function genre_name: string;
    function gross: integer;
    function last_update: string;
    function max_age: integer;
    function min_age: integer;
    function mobi_link_date: string;
    function mobi_link_height: integer;
    function mobi_link_id: integer; // по нему получаем прямой линк на просмотр
    function mobi_link_shift: string;
    function mobi_link_width: integer;
    function mobi_url: string;
    function name_eng: string;
    function name_eng_exact: string;
    function name_eng_exact_t: string;
    function name_id: string;
    function name_original: string;
    function name_original_exact: string;
    function name_original_exact_t: string;
    function name_rus: string;
    function name_rus_exact: string;
    function name_rus_exact_t: string;
    function not_magnet: boolean;
    function partner_episodes_linked: string;
    function persons: string;
    function playable: boolean;
    function popularity: integer;
    function rating_count: integer;
    function rating_imdb: single;
    function rating_imdb_count: integer;
    function rating_kinopoisk: single;
    function rating_kinopoisk_count: integer;
    function release_date: string;
    function release_date_hq: string;
    function rels: string;
    function seeds: integer;
    function strid: string;
    function trailer_id: string;
    function trailer_language: string;
    function trailer_url: string;
    function &type: string;
  end;

  TznItemFull = class(TznCoverMedia, IznItemFull)
    function _version_: int64;
    function adult: boolean;
    function await_count: integer;
    function await_percent: integer;
    function backdrop_id: integer;
    function budget: integer;
    function color_rgb: integer;
    function country: string;
    function country_id: string;
    function description: string;
    function description_eng: string;
    function episodes: string;
    function episodes_linked: string;
    function genre: string;
    function genreId: string;
    function genre_name: string;
    function gross: integer;
    function last_update: string;
    function max_age: integer;
    function min_age: integer;
    function mobi_link_date: string;
    function mobi_link_height: integer;
    function mobi_link_id: integer; // по нему получаем прямой линк на просмотр
    function mobi_link_shift: string;
    function mobi_link_width: integer;
    function mobi_url: string;
    function name_eng: string;
    function name_eng_exact: string;
    function name_eng_exact_t: string;
    function name_id: string;
    function name_original: string;
    function name_original_exact: string;
    function name_original_exact_t: string;
    function name_rus: string;
    function name_rus_exact: string;
    function name_rus_exact_t: string;
    function not_magnet: boolean;
    function partner_episodes_linked: string;
    function persons: string;
    function playable: boolean;
    function popularity: integer;
    function rating_count: integer;
    function rating_imdb: single;
    function rating_imdb_count: integer;
    function rating_kinopoisk: single;
    function rating_kinopoisk_count: integer;
    function release_date: string;
    function release_date_hq: string;
    function rels: string;
    function seeds: integer;
    function strid: string;
    function trailer_id: string;
    function trailer_language: string;
    function trailer_url: string;
    function &type: string;
  end;

implementation

{ TznCoverMedia }

function TznCoverMedia.abuse: string;
begin
  Result := ToSimpleType<string>('abuse');
end;

function TznCoverMedia.audio_quality: integer;
begin
  Result := ToSimpleType<integer>('audio_quality');
end;

function TznCoverMedia.episodes: string;
begin
  Result := ToSimpleType<string>('episodes');
end;

function TznCoverMedia.genre: string;
begin
  Result := ToSimpleType<string>('genre');
end;

function TznCoverMedia.id: string;
begin
  Result := ToSimpleType<string>('id');
end;

function TznCoverMedia.indexed: integer;
begin
  Result := ToSimpleType<integer>('indexed');
end;

function TznCoverMedia.languages_imdb: string;
begin
  Result := ToSimpleType<string>('languages_imdb');
end;

function TznCoverMedia.name_eng: string;
begin
  Result := ToSimpleType<string>('name_eng');
end;

function TznCoverMedia.name_original: string;
begin
  Result := ToSimpleType<string>('name_original');
end;

function TznCoverMedia.name_rus: string;
begin
  Result := ToSimpleType<string>('name_rus');
end;

function TznCoverMedia.playable: boolean;
begin
  Result := ToSimpleType<boolean>('playable');
end;

function TznCoverMedia.quality: integer;
begin
  Result := ToSimpleType<integer>('quality');
end;

function TznCoverMedia.rating: single;
begin
  Result := ToSimpleType<single>('rating');
end;

function TznCoverMedia.release_date_int: string;
begin
  Result := ToSimpleType<string>('release_date_int');
end;

function TznCoverMedia.release_date_rus: string;
begin
  Result := ToSimpleType<string>('release_date_rus');
end;

function TznCoverMedia.runtime: integer;
begin
  Result := ToSimpleType<integer>('runtime');
end;

function TznCoverMedia.serial: boolean;
begin
  Result := ToSimpleType<boolean>('serial');
end;

function TznCoverMedia.serial_ended: boolean;
begin
  Result := ToSimpleType<boolean>('serial_ended');
end;

function TznCoverMedia.serial_end_year: integer;
begin
  Result := ToSimpleType<integer>('serial_end_year');
end;

function TznCoverMedia.tor_count: integer;
begin
  Result := ToSimpleType<integer>('tor_count');
end;

function TznCoverMedia.trailer: boolean;
begin
  Result := ToSimpleType<boolean>('trailer');
end;

function TznCoverMedia.type3d: integer;
begin
  Result := ToSimpleType<integer>('type3d');
end;

function TznCoverMedia.year: integer;
begin
  Result := ToSimpleType<integer>('year');
end;

{ TznItemFull }

function TznItemFull.&type: string;
begin
  Result := ToSimpleType<string>('type');
end;

function TznItemFull.adult: boolean;
begin
  Result := ToSimpleType<boolean>('adult');
end;

function TznItemFull.await_count: integer;
begin
  Result := ToSimpleType<integer>('await_count');
end;

function TznItemFull.await_percent: integer;
begin
  Result := ToSimpleType<integer>('await_percent');
end;

function TznItemFull.backdrop_id: integer;
begin
  Result := ToSimpleType<integer>('backdrop_id');
end;

function TznItemFull.budget: integer;
begin
  Result := ToSimpleType<integer>('budget');
end;

function TznItemFull.color_rgb: integer;
begin
  Result := ToSimpleType<integer>('color_rgb');
end;

function TznItemFull.country: string;
begin
  Result := ToSimpleType<string>('country');
end;

function TznItemFull.country_id: string;
begin
  Result := ToSimpleType<string>('country_id');
end;

function TznItemFull.description: string;
begin
  Result := ToSimpleType<string>('description');

end;

function TznItemFull.description_eng: string;
begin
  Result := ToSimpleType<string>('description_eng');
end;

function TznItemFull.episodes: string;
begin
  Result := ToSimpleType<string>('episodes');
end;

function TznItemFull.episodes_linked: string;
begin
  Result := ToSimpleType<string>('episodes_linked');
end;

function TznItemFull.genre: string;
begin
  Result := ToSimpleType<string>('genre');
end;

function TznItemFull.genreId: string;
begin
  Result := ToSimpleType<string>('genreId');
end;

function TznItemFull.genre_name: string;
begin
  Result := ToSimpleType<string>('genre_name');
end;

function TznItemFull.gross: integer;
begin
  Result := ToSimpleType<integer>('gross');
end;

function TznItemFull.last_update: string;
begin
  Result := ToSimpleType<string>('last_update');
end;

function TznItemFull.max_age: integer;
begin
  Result := ToSimpleType<integer>('max_age');
end;

function TznItemFull.min_age: integer;
begin
  Result := ToSimpleType<integer>('min_age');
end;

function TznItemFull.mobi_link_date: string;
begin
  Result := ToSimpleType<string>('mobi_link_date');
end;

function TznItemFull.mobi_link_height: integer;
begin
  Result := ToSimpleType<integer>('mobi_link_height');
end;

function TznItemFull.mobi_link_id: integer;
begin
  Result := ToSimpleType<integer>('mobi_link_id');
end;

function TznItemFull.mobi_link_shift: string;
begin
  Result := ToSimpleType<string>('mobi_link_shift');
end;

function TznItemFull.mobi_link_width: integer;
begin
  Result := ToSimpleType<integer>('mobi_link_width');
end;

function TznItemFull.mobi_url: string;
begin
  Result := ToSimpleType<string>('mobi_url');
end;

function TznItemFull.name_eng: string;
begin
  Result := ToSimpleType<string>('name_eng');
end;

function TznItemFull.name_eng_exact: string;
begin
  Result := ToSimpleType<string>('name_eng_exact');
end;

function TznItemFull.name_eng_exact_t: string;
begin
  Result := ToSimpleType<string>('name_eng_exact_t');
end;

function TznItemFull.name_id: string;
begin
  Result := ToSimpleType<string>('name_id');
end;

function TznItemFull.name_original: string;
begin
  Result := ToSimpleType<string>('name_original');
end;

function TznItemFull.name_original_exact: string;
begin
  Result := ToSimpleType<string>('name_original_exact');
end;

function TznItemFull.name_original_exact_t: string;
begin
  Result := ToSimpleType<string>('name_original_exact_t');
end;

function TznItemFull.name_rus: string;
begin
  Result := ToSimpleType<string>('name_rus');
end;

function TznItemFull.name_rus_exact: string;
begin
  Result := ToSimpleType<string>('name_rus_exact');
end;

function TznItemFull.name_rus_exact_t: string;
begin
  Result := ToSimpleType<string>('name_rus_exact_t');
end;

function TznItemFull.not_magnet: boolean;
begin
  Result := ToSimpleType<boolean>('not_magnet');
end;

function TznItemFull.partner_episodes_linked: string;
begin
  Result := ToSimpleType<string>('partner_episodes_linked');
end;

function TznItemFull.persons: string;
begin
  Result := ToSimpleType<string>('persons');
end;

function TznItemFull.playable: boolean;
begin
  Result := ToSimpleType<boolean>('playable');
end;

function TznItemFull.popularity: integer;
begin
  Result := ToSimpleType<integer>('popularity');
end;

function TznItemFull.rating_count: integer;
begin
  Result := ToSimpleType<integer>('rating_count');
end;

function TznItemFull.rating_imdb: single;
begin
  Result := ToSimpleType<single>('rating_imdb');
end;

function TznItemFull.rating_imdb_count: integer;
begin
  Result := ToSimpleType<integer>('rating_imdb_count');
end;

function TznItemFull.rating_kinopoisk: single;
begin
  Result := ToSimpleType<single>('rating_kinopoisk');
end;

function TznItemFull.rating_kinopoisk_count: integer;
begin
  Result := ToSimpleType<integer>('rating_kinopoisk_count');
end;

function TznItemFull.release_date: string;
begin
  Result := ToSimpleType<string>('release_date');
end;

function TznItemFull.release_date_hq: string;
begin
  Result := ToSimpleType<string>('release_date_hq');
end;

function TznItemFull.rels: string;
begin
  Result := ToSimpleType<string>('rels');
end;

function TznItemFull.seeds: integer;
begin
  Result := ToSimpleType<integer>('seeds');
end;

function TznItemFull.strid: string;
begin
  Result := ToSimpleType<string>('strid');
end;

function TznItemFull.trailer_id: string;
begin
  Result := ToSimpleType<string>('trailer_id');
end;

function TznItemFull.trailer_language: string;
begin
  Result := ToSimpleType<string>('trailer_language');
end;

function TznItemFull.trailer_url: string;
begin
  Result := ToSimpleType<string>('trailer_url');
end;

function TznItemFull._version_: int64;
begin
  Result := ToSimpleType<int64>('_version_');
end;

end.
