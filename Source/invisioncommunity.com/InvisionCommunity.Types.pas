unit InvisionCommunity.Types;

interface

uses
  InvisionCommunity.Core.JsonBaseClass;

type
  IicArray<T> = interface
    ['{A29496B2-1586-4EDA-90A3-5C98B26C7E08}']
    function Page: Integer;
    function PerPage: Integer;
    function TotalResults: Integer;
    function TotalPages: Integer;
    function Results: TArray<T>;
  end;

  TicArray<T: IInterface> = class(TBaseJson, IicArray<T>)
  private
    FBase: TBaseJsonClass;
  public
    function Page: Integer;
    function PerPage: Integer;
    function TotalResults: Integer;
    function TotalPages: Integer;
    function Results: TArray<T>;
    constructor Create(const AJson: string; TgClass: TBaseJsonClass);
  end;

implementation

uses
  InvisionCommunity.Forums.Types;

{ TicArray<T> }

constructor TicArray<T>.Create(const AJson: string; TgClass: TBaseJsonClass);
begin
  inherited Create(AJson);
  FBase := TgClass;
end;

function TicArray<T>.Page: Integer;
begin
  Result := ReadToSimpleType<Integer>('page');
end;

function TicArray<T>.PerPage: Integer;
begin
  Result := ReadToSimpleType<Integer>('perPage');
end;

function TicArray<T>.Results: TArray<T>;
begin
  Result := ReadToArray<T>(FBase, 'results');
end;

function TicArray<T>.TotalPages: Integer;
begin
  Result := ReadToSimpleType<Integer>('totalPages');
end;

function TicArray<T>.TotalResults: Integer;
begin
  Result := ReadToSimpleType<Integer>('totalResults');
end;

end.

