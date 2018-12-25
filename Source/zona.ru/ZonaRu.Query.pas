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

unit ZonaRu.Query;

interface

uses
  System.Generics.Collections;

type
  TZonaQuery = class
  private
    Fabuse: string;
    Fadult: Boolean;
    Ftor_count: string;
    Findexed: string;
    Fserial: Boolean;
    FNot_genreId: string;
    FID: Int64;
  public
    function ToCatalog: string;
    function ToItem: string;
    constructor Create;
    property id: Int64 read FID write FID;
    property abuse: string read Fabuse write Fabuse;
    property adult: Boolean read Fadult write Fadult;
    property tor_count: string read Ftor_count write Ftor_count;
    property indexed: string read Findexed write Findexed;
    property serial: Boolean read Fserial write Fserial;
    property Not_genreId: string read FNot_genreId write FNot_genreId;
  end;

  TznSortType = (ByRaiting);

  TznField = (  //
    seeds, popularity, rating_count, add_date, release_date, //
    id, year, playable, trailer, quality, audio_quality, type3d, serial,
    languages_imdb, rating, genre2, runtime, episodes, tor_count,
    serial_end_year, serial_ended, abuse, release_date_int, release_date_rus,
    indexed, geo_rules, partner_entity_id, partner_type, name_rus, name_ukr, name_eng, name_original);

  TznFields = set of TznField;

  TZonaQueryEx = class
  private
    FFields: TznFields;
    FStart: Integer;
    FRows: Integer;
    Fwt: string;
    FVersion: string;
    FIsSerial: Boolean;
    FBlackGenres: TList<Integer>;
    FWhiteGenres: TList<Integer>;
  protected
    function FieldToString(const AField: TznField): string;
    function FieldsToString(const AFields: TznFields): string;
  public
    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function ToString: string; override;
    
  published
    property IsSerial: Boolean read FIsSerial write FIsSerial;
    property Fields: TznFields read FFields write FFields;
    property Start: Integer read FStart write FStart;
    property Rows: Integer read FRows write FRows;
    property wt: string read Fwt;
    property Version: string read FVersion;
    property WhiteGenres: TList<Integer> read FWhiteGenres write FWhiteGenres;
    property BlackGenres: TList<Integer> read FBlackGenres write FBlackGenres;
  end;

implementation

uses
  System.TypInfo,
  System.SysUtils;

type
  TEnumConverter = class
  public
    class function EnumToInt<T>(const EnumValue: T): Integer;
    class function EnumToString<T>(EnumValue: T): string;
  end;
{ TZonaQuery }

constructor TZonaQuery.Create;
begin
  abuse := 'zona';
  adult := False;
  tor_count := '[1+TO+2147483647]';
  indexed := '[1+TO+7]';
  serial := False;
  Not_genreId := '(12+OR+15+OR+25+OR+26+OR+1747+OR+28+OR+27+OR+tv)';
  FID := 0;
end;

function TZonaQuery.ToCatalog: string;
begin
  Result :=
    '(NOT(abuse:%abuse%)AND(adult:%adult%)AND(tor_count:%tor_count%)AND(indexed:%indexed%)AND(serial:%serial%)NOT(genreId:%genreId%))';
  Result := Result //
    .Replace('%abuse%', abuse) //
    .Replace('%adult%', adult.ToString(TUseBoolStrs.True).ToLower) //
    .Replace('%tor_count%', tor_count) //
    .Replace('%indexed%', indexed) //
    .Replace('%serial%', serial.ToString(TUseBoolStrs.True).ToLower) //
    .Replace('%genreId%', Not_genreId) //
;
end;

function TZonaQuery.ToItem: string;
begin
  Result := '((id:%ID%))'.Replace('%ID%', id.ToString);
end;

{ TZonaQueryEx }

procedure TZonaQueryEx.AfterConstruction;
begin
  inherited;
  FStart := 0;
  FRows := 60;
  Fwt := 'json2';
  FVersion := '2.2';
  FFields := [TznField(Low(TznField))..TznField(High(TznField))];
end;

constructor TZonaQueryEx.Create;
begin
  FWhiteGenres := TList<Integer>.Create;
  FBlackGenres := TList<Integer>.Create;
end;

destructor TZonaQueryEx.Destroy;
begin
  FWhiteGenres.Free;
  FBlackGenres.Free;
  inherited;
end;

function TZonaQueryEx.FieldsToString(const AFields: TznFields): string;
var
  AField: TznField;
begin
  for AField in AFields do
  begin
    Result := Result + FieldToString(AField) + ',';
  end;
  Result := Result.Remove(Result.Length - 2, 1);
end;

function TZonaQueryEx.FieldToString(const AField: TznField): string;
begin
  Result := TEnumConverter.EnumToString<TznField>(AField);
end;

function TZonaQueryEx.ToString: string;
begin
//(NOT(abuse:zona)AND(tor_count:[1 TO 2147483647])AND(indexed:[1 TO 7])AND(serial:false)AND(genreId:(20))NOT(genreId:(12 OR 15 OR 25 OR 26 OR 1747 OR 28 OR 27)))
end;

{ TEnumConverter }

class function TEnumConverter.EnumToInt<T>(const EnumValue: T): Integer;
begin
  Result := 0;
  Move(EnumValue, Result, SizeOf(EnumValue));
end;

class function TEnumConverter.EnumToString<T>(EnumValue: T): string;
begin
  Result := GetEnumName(TypeInfo(T), EnumToInt(EnumValue));
end;

end.

