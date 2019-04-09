{
  This file is part of CrossVCL WebBroser
  Copyright (c) 2018 Eugene Kryukov

  The contents of this file are subject to the Mozilla Public License Version 2.0 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
  KIND, either express or implied. See the License for the specific language governing rights and
  limitations under the License.
}

unit CVcl.WebView.Mac;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, CVcl.WebView, Macapi.ObjCRuntime, Macapi.ObjectiveC,
  Macapi.Helpers, Macapi.Foundation, Macapi.AppKit, Macapi.WebKit;

type

  TMacWebView = class;

  WebFrameLoadDelegate = interface(IObjectiveC)
    ['{50FBA900-6373-4D55-89A0-21BC30D8B033}']
    procedure webViewDidFinishLoadForFrame(sender: WebView; didFinishLoadForFrame: WebFrame); cdecl;
    procedure webViewDidFailLoadWithErrorForFrame(sender: WebView; didFailLoadWithError: NSError; forFrame: WebFrame); cdecl;
  end;

  TCocoaDelegate = class(TOCLocal, WebFrameLoadDelegate)
  private
    FDelegate: IWebViewDelegate;
  public
    constructor Create(ADelegate: IWebViewDelegate);
    { WebFrameLoadDelegate }
    [MethodName('webView:didFinishLoadForFrame:')]
    procedure webViewDidFinishLoadForFrame(sender: WebView; didFinishLoadForFrame: WebFrame); cdecl;
    [MethodName('webView:didFailLoadWithError:forFrame:')]
    procedure webViewDidFailLoadWithErrorForFrame(sender: WebView; didFailLoadWithError: NSError; forFrame: WebFrame); cdecl;
  end;

  TMacWebView = class(TInterfacedObject, IWebView)
  private
    FWebView: WebView;
    FParentWnd: HWnd;
    FDelegate: IWebViewDelegate;
    FCocoaDelegate: TCocoaDelegate;
    { IWebView }
    procedure WebViewUpdateBounds;
    procedure WebViewNavigate(const URL: string);
    procedure WebViewLoadFromStream(const AStream: TStream; const MimeType, BaseUrl: string);
    procedure WebViewLoadFromString(const AString: string; const BaseUrl: string);
    function WebViewRunScript(const AScript: string; var ResultStr, ErrorStr: string): Boolean;
    function WebViewGetSelectedText: string;
    procedure WebViewGoForward;
    procedure WebViewGoBack;
    function WebGetHandle: Pointer;
  protected
  public
    constructor Create(AParentWnd: HWnd; ADelegate: IWebViewDelegate);
    destructor Destroy; override;
  end;

implementation

{ TCocoaDelegate }

constructor TCocoaDelegate.Create(ADelegate: IWebViewDelegate);
begin
  inherited Create;
  FDelegate := ADelegate;
end;

procedure TCocoaDelegate.webViewDidFailLoadWithErrorForFrame(sender: WebView;
  didFailLoadWithError: NSError; forFrame: WebFrame);
begin
//  FDelegate.FinishLoading;
end;

procedure TCocoaDelegate.webViewDidFinishLoadForFrame(sender: WebView; didFinishLoadForFrame: WebFrame);
begin
  FDelegate.FinishLoading;
end;

function maintainsInactiveSelection(self: Pointer; _cmd: SEL): Boolean; cdecl;
begin
  Result := True;
end;

procedure ReplaceMethods(WebView: Pointer);
var
  FrameClass: Pointer;
  M1, M2: Pointer;
begin
  // Replace drawRect method on window frame
  FrameClass := object_getClass(WebView);
  class_addMethod(FrameClass, sel_getUid('maintainsInactiveSelectionOriginal'), @maintainsInactiveSelection, 'v@');
  M1 := class_getInstanceMethod(FrameClass, sel_getUid('maintainsInactiveSelection'));
  M2 := class_getInstanceMethod(FrameClass, sel_getUid('maintainsInactiveSelectionOriginal'));
  method_exchangeImplementations(M1, M2);
end;

{ TMacWebView }

constructor TMacWebView.Create(AParentWnd: HWnd; ADelegate: IWebViewDelegate);
var
  WindowPtr, ContentViewPtr: Pointer;
  ContentView: NSView;
begin
  inherited Create;
  FDelegate := ADelegate;
  FParentWnd := AParentWnd;

  FWebView := TWebView.Create;
  ReplaceMethods((FWebView as ILocalObject).GetObjectID);

  FCocoaDelegate := TCocoaDelegate.Create(FDelegate);
  FWebView.setFrameLoadDelegate(FCocoaDelegate.GetObjectID);

  if GetNativeHandles(FParentWnd, WindowPtr, ContentViewPtr) then
  begin
    ContentView := TNSView.Wrap(ContentViewPtr);

    SetNativeParent((FWebView as ILocalObject).GetObjectID, FParentWnd);

    FWebView.setFrame(ContentView.bounds);
    FWebView.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
  end;
end;

destructor TMacWebView.Destroy;
begin
  FWebView.release;
  inherited;
end;

function TMacWebView.WebGetHandle: Pointer;
begin
  Result := FWebView;
end;

function TMacWebView.WebViewGetSelectedText: string;
var
  Error: string;
begin
  WebViewRunScript('window.getSelection().toString()', Result, Error);
end;

procedure TMacWebView.WebViewGoBack;
begin
  FWebView.goBack;
end;

procedure TMacWebView.WebViewGoForward;
begin
  FWebView.goForward;
end;

procedure TMacWebView.WebViewLoadFromStream(const AStream: TStream; const MimeType, BaseUrl: string);
var
  Memory: TMemoryStream;
  Data: NSData;
begin
  Memory := TMemoryStream.Create;
  try
    Memory.CopyFrom(AStream, AStream.Size);
    Data := TNSData.Wrap(TNSData.OCClass.dataWithBytes(Memory.Memory, Memory.Size));
    FWebView.mainFrame.loadData(Data, NSStr(MimeType), nil, StrToNSUrl(BaseUrl));
  finally
    Memory.Free;
  end;
end;

procedure TMacWebView.WebViewLoadFromString(const AString: string; const BaseUrl: string);
begin
  FWebView.mainFrame.loadHTMLString(NSStr(AString), StrToNSUrl(BaseUrl));
end;

procedure TMacWebView.WebViewNavigate(const URL: string);
var
  URLRequest: NSURLRequest;
  URLStr: NSUrl;
begin
  URLStr := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(StrToNSStr(URL)));
  URLRequest := TNSURLRequest.Wrap(TNSURLRequest.OCClass.requestWithURL(URLStr));
  FWebView.mainFrame.loadRequest(URLRequest);
end;

function TMacWebView.WebViewRunScript(const AScript: string; var ResultStr,
  ErrorStr: string): Boolean;
var
  LJavaScript: NSString;
begin
  LJavaScript := StrToNSStr(AScript);
  ResultStr := NSStrToStr(FWebView.stringByEvaluatingJavaScriptFromString(LJavaScript));
  Result := ResultStr <> '';
end;

procedure TMacWebView.WebViewUpdateBounds;
begin
  // Updated by Cocoa
end;

end.
