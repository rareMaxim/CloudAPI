unit InvisionCommunity.Forums;

interface

uses
  InvisionCommunity.Core.Api,
  InvisionCommunity.Types,
  InvisionCommunity.Forums.Types;

type
  IicForums = interface(IicRequest)
    ['{4AA39BC9-A179-4C20-9A90-CF1D3A86E34E}']
    function GetForums: IicArray<IicForumObject>;
    function GetTopics(const Forums, authors: string; //
      const hasBestAnswer, hasPoll, locked, hidden, pinned, featured, archived: Integer; //
      const sortBy, sortDir: string; //
      const page: Integer //
    ): IicArray<IicTopicObject>;
  end;

  TicForums = class(TicRequest, IicForums)
    function GetForums: IicArray<IicForumObject>;
    function GetTopics(const Forums, authors: string; //
      const hasBestAnswer, hasPoll, locked, hidden, pinned, featured, archived: Integer; //
      const sortBy, sortDir: string; //
      const page: Integer //
    ): IicArray<IicTopicObject>;
  end;

implementation

uses
  System.Net.URLClient;

{ TicSystem }

function TicForums.GetForums: IicArray<IicForumObject>;
begin
  SetPath('/api/forums/forums');
  Result := TicArray<IicForumObject>.Create(Get, TicForumObject);
end;

function TicForums.GetTopics( //
  const Forums, authors: string; //
  const hasBestAnswer, hasPoll, locked, hidden, pinned, featured, archived: Integer; //
  const sortBy, sortDir: string; //
  const page: Integer): IicArray<IicTopicObject>;
begin
  AddParameter('forums', Forums);
  AddParameter('authors', authors);
  AddParameter('hasBestAnswer', hasBestAnswer);
  AddParameter('hasPoll', hasPoll);
  AddParameter('locked', locked);
  AddParameter('hidden', hidden);
  AddParameter('pinned', pinned);
  AddParameter('featured', featured);
  AddParameter('archived', archived);
  AddParameter('sortBy', sortBy);
  AddParameter('sortDir', sortDir);
  AddParameter('page', page);
  SetPath('/api/forums/topics');
  Result := TicArray<IicTopicObject>.Create(Get, TicTopicObject);
end;

end.

