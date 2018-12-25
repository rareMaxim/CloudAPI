{***************************************************************************}
{                                                                           }
{           CloudApi for Delphi                                             }
{                                                                           }
{           Copyright (c) 2014-2018 Maxim Sysoev                            }
{                                                                           }
{           https://t.me/CloudAPI                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit ZonaRu.Types;

interface

uses
  CloudAPI.Utils.Json;

type
  IznLastResponse = interface
    ['{4735F71E-A681-4A11-AB58-983383CA12FF}']
    function numFound: Integer;
    function start: Integer;
  end;

  TznLastResponse = class(TBaseJson, IznLastResponse)
    function numFound: Integer;
    function start: Integer;
  end;

  IznCoverFilm = interface
    ['{2DF78C63-B657-41DE-99FC-173EB5AEB3CB}']
    function abuse: string;
    function audio_quality: Integer;
    function genre: string;
    function id: string;
    function indexed: Integer;
    function languages_imdb: string;
    function name_eng: string;
    function name_original: string;
    function name_rus: string;
    function playable: Boolean;
    function quality: Integer;
    function rating: Single;
    function release_date_int: string;
    function release_date_rus: string;
    function runtime: Integer;
    function serial: Boolean;
    function serial_ended: Boolean;
    function serial_end_year: Integer;
    function tor_count: Integer;
    function trailer: Boolean;
    function type3d: Integer;
    function year: Integer;
  end;

  TznCoverFilm = class(TBaseJson, IznCoverFilm)
  public
    function abuse: string;
    function audio_quality: Integer;
    function genre: string;
    function id: string;
    function indexed: Integer;
    function languages_imdb: string;
    function name_eng: string;
    function name_original: string;
    function name_rus: string;
    function playable: Boolean;
    function quality: Integer;
    function rating: Single;
    function release_date_int: string;
    function release_date_rus: string;
    function runtime: Integer;
    function serial: Boolean;
    function serial_ended: Boolean;
    function serial_end_year: Integer;
    function tor_count: Integer;
    function trailer: Boolean;
    function type3d: Integer;
    function year: Integer;
  end;

  IznCoverSerial = interface(IznCoverFilm)
    ['{27D4ABA7-59C1-4B23-8091-40F0BE5D0897}']
    function episodes: string;
  end;

  TznCoverSerial = class(TznCoverFilm, IznCoverSerial)
  public
    function episodes: string;
  end;

  IznItemFull = interface(IznCoverFilm)
    ['{A80FA048-ED6D-49CA-A782-9A2D4C549247}']
    function _version_: Int64;
    function adult: Boolean;
    function await_count: Integer;
    function await_percent: Integer;
    function backdrop_id: Integer;
    function budget: Integer;
    function color_rgb: Integer;
    function country: string;
    function country_id: string;
    function description: string;
    function description_eng: string;
    function episodes: string;
    function episodes_linked: string;
    function genre: string;
    function genreId: string;
    function genre_name: string;
    function gross: Integer;
    function last_update: string;
    function max_age: Integer;
    function min_age: Integer;
    function mobi_link_date: string;
    function mobi_link_height: Integer;
    function mobi_link_id: Integer; // по нему получаем прямой линк на просмотр
    function mobi_link_shift: string;
    function mobi_link_width: Integer;
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
    function not_magnet: Boolean;
    function partner_episodes_linked: string;
    function persons: string;
    function playable: Boolean;
    function popularity: Integer;
    function rating_count: Integer;
    function rating_imdb: Single;
    function rating_imdb_count: Integer;
    function rating_kinopoisk: Single;
    function rating_kinopoisk_count: Integer;
    function release_date: string;
    function release_date_hq: string;
    function rels: string;
    function seeds: Integer;
    function strid: string;
    function trailer_id: string;
    function trailer_language: string;
    function trailer_url: string;
    function &type: string;
  end;

  TznItemFull = class(TznCoverFilm, IznItemFull)
    function _version_: Int64;
    function adult: Boolean;
    function await_count: Integer;
    function await_percent: Integer;
    function backdrop_id: Integer;
    function budget: Integer;
    function color_rgb: Integer;
    function country: string;
    function country_id: string;
    function description: string;
    function description_eng: string;
    function episodes: string;
    function episodes_linked: string;
    function genre: string;
    function genreId: string;
    function genre_name: string;
    function gross: Integer;
    function last_update: string;
    function max_age: Integer;
    function min_age: Integer;
    function mobi_link_date: string;
    function mobi_link_height: Integer;
    function mobi_link_id: Integer; // по нему получаем прямой линк на просмотр
    function mobi_link_shift: string;
    function mobi_link_width: Integer;
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
    function not_magnet: Boolean;
    function partner_episodes_linked: string;
    function persons: string;
    function playable: Boolean;
    function popularity: Integer;
    function rating_count: Integer;
    function rating_imdb: Single;
    function rating_imdb_count: Integer;
    function rating_kinopoisk: Single;
    function rating_kinopoisk_count: Integer;
    function release_date: string;
    function release_date_hq: string;
    function rels: string;
    function seeds: Integer;
    function strid: string;
    function trailer_id: string;
    function trailer_language: string;
    function trailer_url: string;
    function &type: string;
  end;

  IznDirectMediaInfo = interface
    ['{B7FF9024-167C-41D7-84F8-D424C8500932}']
    function Images: TArray<string>;
    function url: string;
  end;

  TznDirectMediaInfo = class(TBaseJson, IznDirectMediaInfo)
  public
    function Images: System.TArray<System.string>;
    function url: string;
  end;

implementation

{ TznCoverMedia }

function TznCoverFilm.abuse: string;
begin
  Result := ToSimpleType<string>('abuse');
end;

function TznCoverFilm.audio_quality: Integer;
begin
  Result := ToSimpleType<Integer>('audio_quality');
end;

function TznCoverFilm.genre: string;
begin
  Result := ToSimpleType<string>('genre2');
end;

function TznCoverFilm.id: string;
begin
  Result := ToSimpleType<string>('id');
end;

function TznCoverFilm.indexed: Integer;
begin
  Result := ToSimpleType<Integer>('indexed');
end;

function TznCoverFilm.languages_imdb: string;
begin
  Result := ToSimpleType<string>('languages_imdb');
end;

function TznCoverFilm.name_eng: string;
begin
  Result := ToSimpleType<string>('name_eng');
end;

function TznCoverFilm.name_original: string;
begin
  Result := ToSimpleType<string>('name_original');
end;

function TznCoverFilm.name_rus: string;
begin
  Result := ToSimpleType<string>('name_rus');
end;

function TznCoverFilm.playable: Boolean;
begin
  Result := ToSimpleType<Boolean>('playable');
end;

function TznCoverFilm.quality: Integer;
begin
  Result := ToSimpleType<Integer>('quality');
end;

function TznCoverFilm.rating: Single;
begin
  Result := ToSimpleType<Single>('rating');
end;

function TznCoverFilm.release_date_int: string;
begin
  Result := ToSimpleType<string>('release_date_int');
end;

function TznCoverFilm.release_date_rus: string;
begin
  Result := ToSimpleType<string>('release_date_rus');
end;

function TznCoverFilm.runtime: Integer;
begin
  Result := ToSimpleType<Integer>('runtime');
end;

function TznCoverFilm.serial: Boolean;
begin
  Result := ToSimpleType<Boolean>('serial');
end;

function TznCoverFilm.serial_ended: Boolean;
begin
  Result := ToSimpleType<Boolean>('serial_ended');
end;

function TznCoverFilm.serial_end_year: Integer;
begin
  Result := ToSimpleType<Integer>('serial_end_year');
end;

function TznCoverFilm.tor_count: Integer;
begin
  Result := ToSimpleType<Integer>('tor_count');
end;

function TznCoverFilm.trailer: Boolean;
begin
  Result := ToSimpleType<Boolean>('trailer');
end;

function TznCoverFilm.type3d: Integer;
begin
  Result := ToSimpleType<Integer>('type3d');
end;

function TznCoverFilm.year: Integer;
begin
  Result := ToSimpleType<Integer>('year');
end;

{ TznItemFull }

function TznItemFull.&type: string;
begin
  Result := ToSimpleType<string>('type');
end;

function TznItemFull.adult: Boolean;
begin
  Result := ToSimpleType<Boolean>('adult');
end;

function TznItemFull.await_count: Integer;
begin
  Result := ToSimpleType<Integer>('await_count');
end;

function TznItemFull.await_percent: Integer;
begin
  Result := ToSimpleType<Integer>('await_percent');
end;

function TznItemFull.backdrop_id: Integer;
begin
  Result := ToSimpleType<Integer>('backdrop_id');
end;

function TznItemFull.budget: Integer;
begin
  Result := ToSimpleType<Integer>('budget');
end;

function TznItemFull.color_rgb: Integer;
begin
  Result := ToSimpleType<Integer>('color_rgb');
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

function TznItemFull.gross: Integer;
begin
  Result := ToSimpleType<Integer>('gross');
end;

function TznItemFull.last_update: string;
begin
  Result := ToSimpleType<string>('last_update');
end;

function TznItemFull.max_age: Integer;
begin
  Result := ToSimpleType<Integer>('max_age');
end;

function TznItemFull.min_age: Integer;
begin
  Result := ToSimpleType<Integer>('min_age');
end;

function TznItemFull.mobi_link_date: string;
begin
  Result := ToSimpleType<string>('mobi_link_date');
end;

function TznItemFull.mobi_link_height: Integer;
begin
  Result := ToSimpleType<Integer>('mobi_link_height');
end;

function TznItemFull.mobi_link_id: Integer;
begin
  Result := ToSimpleType<Integer>('mobi_link_id');
end;

function TznItemFull.mobi_link_shift: string;
begin
  Result := ToSimpleType<string>('mobi_link_shift');
end;

function TznItemFull.mobi_link_width: Integer;
begin
  Result := ToSimpleType<Integer>('mobi_link_width');
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

function TznItemFull.not_magnet: Boolean;
begin
  Result := ToSimpleType<Boolean>('not_magnet');
end;

function TznItemFull.partner_episodes_linked: string;
begin
  Result := ToSimpleType<string>('partner_episodes_linked');
end;

function TznItemFull.persons: string;
begin
  Result := ToSimpleType<string>('persons');
end;

function TznItemFull.playable: Boolean;
begin
  Result := ToSimpleType<Boolean>('playable');
end;

function TznItemFull.popularity: Integer;
begin
  Result := ToSimpleType<Integer>('popularity');
end;

function TznItemFull.rating_count: Integer;
begin
  Result := ToSimpleType<Integer>('rating_count');
end;

function TznItemFull.rating_imdb: Single;
begin
  Result := ToSimpleType<Single>('rating_imdb');
end;

function TznItemFull.rating_imdb_count: Integer;
begin
  Result := ToSimpleType<Integer>('rating_imdb_count');
end;

function TznItemFull.rating_kinopoisk: Single;
begin
  Result := ToSimpleType<Single>('rating_kinopoisk');
end;

function TznItemFull.rating_kinopoisk_count: Integer;
begin
  Result := ToSimpleType<Integer>('rating_kinopoisk_count');
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

function TznItemFull.seeds: Integer;
begin
  Result := ToSimpleType<Integer>('seeds');
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

function TznItemFull._version_: Int64;
begin
  Result := ToSimpleType<Int64>('_version_');
end;

{ TznLastResponse }

function TznLastResponse.numFound: Integer;
begin
  Result := ToSimpleType<Integer>('numFound');
end;

function TznLastResponse.start: Integer;
begin
  Result := ToSimpleType<Integer>('start');
end;

{ TznDirectMediaInfo }

function TznDirectMediaInfo.Images: System.TArray<System.string>;
begin
  TBaseJson.UnSupported;
end;

function TznDirectMediaInfo.url: string;
begin
  Result := ToSimpleType<string>('url');
end;

{ TznCoverSerial }

function TznCoverSerial.episodes: string;
begin
  Result := ToSimpleType<string>('episodes');
end;

end.

