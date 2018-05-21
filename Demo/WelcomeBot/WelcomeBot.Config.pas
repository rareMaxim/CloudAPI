unit WelcomeBot.Config;

interface

type
  TwbConfig = class
  private
    FToken: string;
    FWelcomeText: string;
  public
    property WelcomeText: string read FWelcomeText write FWelcomeText;
    property Token: string read FToken write FToken;
  end;

implementation

end.

