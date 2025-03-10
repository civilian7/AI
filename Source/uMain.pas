unit uMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Grids,

  // https://github.com/MaxiDonkey/DelphiAnthropic
  Anthropic,
  Anthropic.Types,
  Anthropic.Tutorial.VCL;

type
  TfrmMain = class(TForm)
    lbSystem: TLabel;
    eSystem: TEdit;
    btnExecute: TButton;
    eResult: TMemo;
    btnCancel: TButton;
    eQuestion: TEdit;
    lbQuestion: TLabel;
    OpenDialog1: TOpenDialog;
    eImage: TImage;
    btnTextQuestion: TButton;
    btnBugs: TButton;
    btnComplete: TButton;
    procedure btnExecuteClick(Sender: TObject);
    procedure btnTextQuestionClick(Sender: TObject);
    procedure btnBugsClick(Sender: TObject);
    procedure btnCompleteClick(Sender: TObject);
  private
    Claude: IAnthropic;

    procedure CreateEngine;
    procedure Extract(const AFileName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  Vcl.Imaging.Jpeg,
  Vcl.Imaging.pngimage;

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;

  CreateEngine;
end;

destructor TfrmMain.Destroy;
begin
  inherited;
end;

procedure TfrmMain.CreateEngine;
begin
  TutorialHub := TVCLTutorialHub.Create(eResult, btnCancel);
  var LKey := TEncoding.ANSI.GetString(TFile.ReadAllBytes('APIKey.txt'));
  Claude := TAnthropicFactory.CreateInstance(LKey);
end;

procedure TfrmMain.btnTextQuestionClick(Sender: TObject);
begin
  Claude.Chat.AsynCreate(
    procedure (Params: TChatParams)
    begin
      Params.Model('claude-3-7-sonnet-20250219');
      Params.MaxTokens(20000);

      Params.System(eSystem.Text);
      Params.Messages([
        FromUser(eQuestion.Text)
      ]);
    end,
    function : TAsynChat
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end
  );
end;

procedure TfrmMain.btnBugsClick(Sender: TObject);
begin
  var LList := TStringList.Create;
  LList.LoadFromFile('c:\temo\1.txt');

{//************ Find Bugs ***************
# Bugs in the Delphi Code

The provided Delphi code has several issues that could lead to bugs or problems:

1. **Missing try/finally block**: The code creates a `TStringList` instance but doesn't properly free it, which will cause a memory leak. In Delphi, objects created with `.Create` must be explicitly freed.

2. **Typo in file path**: There's a typo in the file path ('c:\temo\1.txt' instead of likely 'c:\temp\1.txt').

3. **No error handling**: The code doesn't handle potential exceptions from `LoadFromFile`, such as when the file doesn't exist or is inaccessible.

4. **Hard-coded file path**: Using an absolute hard-coded path is generally not good practice as it creates deployment issues.

Here's a corrected version:

```delphi
var
  LList: TStringList;
begin
  LList := TStringList.Create;
  try
    try
      LList.LoadFromFile('c:\temp\1.txt');
      // Do something with LList here
    except
      on E: Exception do
        ShowMessage('Error loading file: ' + E.Message);
    end;
  finally
    LList.Free;
  end;
end;
```

This version:
- Properly declares the variable
- Uses a try/finally block to ensure memory cleanup
- Adds exception handling
- Corrects the likely path typo
}
end;

procedure TfrmMain.btnCompleteClick(Sender: TObject);
begin
  var LList := TStringList.Create;
{//************ Complete Code ***************
Here's the completed Delphi code with proper object lifecycle management:

```delphi
var
  LList: TStringList;
begin
  LList := TStringList.Create;
  try
    // Use the string list here
    // For example:
    // LList.Add('Item 1');
    // LList.Add('Item 2');
    // LList.SaveToFile('myfile.txt');
  finally
    LList.Free; // Properly dispose of the object when done
  end;
end;
```

This code pattern uses the try-finally structure which is essential in Delphi for proper resource management. The finally block ensures that the TStringList object is freed even if an exception occurs while working with it. This prevents memory leaks.

If you need the code for a specific purpose, I can customize it further with more relevant operations for your particular use case.
}
end;

procedure TfrmMain.btnExecuteClick(Sender: TObject);
begin
  if OpenDialog1.Execute(Handle) then
  begin
    eImage.Picture.LoadFromFile(OpenDialog1.FileName);
    Extract(OpenDialog1.FileName);
  end;

{//************ Add Comment ***************
```pascal
// Display a file open dialog, passing the current window handle to ensure proper modal behavior
if OpenDialog1.Execute(Handle) then
begin
  // Load the selected image file into the eImage control's Picture property
  eImage.Picture.LoadFromFile(OpenDialog1.FileName);

  // Call the Extract procedure to process the selected file
  // This likely extracts information or content from the image file
  Extract(OpenDialog1.FileName);
end;
```
}




{//************ Explanation ***************
# Code Explanation

이 코드는 델파이(객체 파스칼)로 작성된 것으로 보이며 이미지 파일의 선택 및 로딩을 처리합니다. 한 줄 한 줄 분석해 보겠습니다:

```pascal
if OpenDialog1.Execute(Handle) then
begin
  eImage.Picture.LoadFromFile(OpenDialog1.FileName);
  Extract(OpenDialog1.FileName);
end;
```

## What this code does:

1. **`if OpenDialog1.Execute(Handle) then`**
   - `OpenDialog1` is an instance of a file open dialog component
   - `.Execute(Handle)` displays the standard Windows "Open File" dialog to the user, with `Handle` specifying the parent window handle
   - This method returns `true` if the user selects a file and clicks "Open", and `false` if they cancel

2. **Inside the `begin`/`end` block (only executed if a file was selected):**

   - **`eImage.Picture.LoadFromFile(OpenDialog1.FileName);`**
     - `eImage` appears to be an image control (like TImage)
     - Loads the selected file (specified by `OpenDialog1.FileName`) into the image control's Picture property
     - This displays the selected image on the form

   - **`Extract(OpenDialog1.FileName);`**
     - Calls a function named `Extract` and passes the selected filename to it
     - This function is not defined in the snippet, but likely performs some operation on the selected image file (such as extracting data, metadata, or performing image processing)

## Summary:
This code displays a file open dialog, and if the user selects a file, it displays that image in an image control and then processes the image through an `Extract` function.
}
end;

procedure TfrmMain.Extract(const AFileName: string);
begin
  Claude.Chat.AsynCreate(
    procedure (Params: TChatParams)
    begin
      Params.Model('claude-3-7-sonnet-20250219');
      Params.MaxTokens(20000);
      Params.Messages([
        FromUser('영수증에서 일자, 상호, 총금액을 뽑아서 json 포맷으로 정리해줘', [AFileName], True)
      ]);
    end,
    function : TAsynChat
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end
  );
end;

end.
