unit InvisionCommunity.Exceptions;

interface

uses
  System.SysUtils;

type
  TicExcception = class(Exception)
  private
    FCode: string;
  public
    constructor Create(const Code, Msg: string); overload;
  published
    property Code: string read FCode write FCode;
  end;

implementation

{ TInvisionCommunityExcception }

constructor TicExcception.Create(const Code, Msg: string);
begin
  inherited Create(Msg);
  FCode := Code;
end;

end.

