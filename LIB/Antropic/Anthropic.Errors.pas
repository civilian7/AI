unit Anthropic.Errors;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  REST.Json.Types;

type
  TErrorCore = class abstract
  end;

  TErrorObject = class
  private
    [JsonNameAttribute('type')]
    FType: string;
    [JsonNameAttribute('message')]
    FMessage: string;
  public
    property &Type: string read FType write FType;
    property Message: string read FMessage write FMessage;
  end;

  TError = class(TErrorCore)
  private
    [JsonNameAttribute('type')]
    FType: string;
    [JsonNameAttribute('error')]
    FError: TErrorObject;
  public
    property &Type: string read FType write FType;
    property Error: TErrorObject read FError write FError;
    destructor Destroy; override;
  end;

implementation

{ TError }

destructor TError.Destroy;
begin
  if Assigned(FError) then
    FError.Free;
  inherited;
end;

end.
