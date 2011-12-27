%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% File: util.erl
%% Date: 25/08/2010
%%
%% @doc MIME utility functions
%%
%% @author Zvi Avraham <zvi@nivertech.com>
%% @copyright 2010-2011 Nivertech (Nywhere Tech Ltd)
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(util_mime).

-export([guess_mime/1,
         from_ext/1]).

%%------------------------------------------------------------------------------
%% @doc  Guess the mime type of a file by the extension of its filename.
%% @end
%%------------------------------------------------------------------------------
-spec guess_mime(Filename::string()) -> binary().
guess_mime(Filename) ->
    ExtWithoutDot = case filename:extension(Filename) of
                        [$.|Ext] -> Ext;
                        OtherExt -> OtherExt
                    end,
    case from_ext(list_to_binary(ExtWithoutDot)) of
        undefined   -> <<"text/plain">>;
        Mime        -> Mime
    end.

%%------------------------------------------------------------------------------
%% @doc Guess MIME type from filename extension, i.e. 
%%        <<"json">>)  =>  <<"application/json">>
%%      Return atom undefined if no good guess is available.
%% @end
%%------------------------------------------------------------------------------
-spec from_ext(FileExt::binary()) -> binary() | undefined.
from_ext(<<>>)             -> <<>>;
from_ext(<<"stl">>)        -> <<"application/SLA">>;
from_ext(<<"stp">>)        -> <<"application/STEP">>;
from_ext(<<"step">>)       -> <<"application/STEP">>;
from_ext(<<"dwg">>)        -> <<"application/acad">>;
from_ext(<<"ez">>)         -> <<"application/andrew-inset">>;
from_ext(<<"ccad">>)       -> <<"application/clariscad">>;
from_ext(<<"drw">>)        -> <<"application/drafting">>;
from_ext(<<"tsp">>)        -> <<"application/dsptype">>;
from_ext(<<"dxf">>)        -> <<"application/dxf">>;
from_ext(<<"xls">>)        -> <<"application/excel">>;
from_ext(<<"unv">>)        -> <<"application/i-deas">>;
from_ext(<<"jar">>)        -> <<"application/java-archive">>;
from_ext(<<"hqx">>)        -> <<"application/mac-binhex40">>;
from_ext(<<"cpt">>)        -> <<"application/mac-compactpro">>;
from_ext(<<"pot">>)        -> <<"application/vnd.ms-powerpoint">>;
from_ext(<<"ppt">>)        -> <<"application/vnd.ms-powerpoint">>;
from_ext(<<"dms">>)        -> <<"application/octet-stream">>;
from_ext(<<"lha">>)        -> <<"application/octet-stream">>;
from_ext(<<"lzh">>)        -> <<"application/octet-stream">>;
from_ext(<<"oda">>)        -> <<"application/oda">>;
from_ext(<<"ogg">>)        -> <<"application/ogg">>;
from_ext(<<"ogm">>)        -> <<"application/ogg">>;
from_ext(<<"pdf">>)        -> <<"application/pdf">>;
from_ext(<<"pgp">>)        -> <<"application/pgp">>;
from_ext(<<"ai">>)         -> <<"application/postscript">>;
from_ext(<<"eps">>)        -> <<"application/postscript">>;
from_ext(<<"ps">>)         -> <<"application/postscript">>;
from_ext(<<"prt">>)        -> <<"application/pro_eng">>;
from_ext(<<"rtf">>)        -> <<"application/rtf">>;
from_ext(<<"smi">>)        -> <<"application/smil">>;
from_ext(<<"smil">>)       -> <<"application/smil">>;
from_ext(<<"sol">>)        -> <<"application/solids">>;
from_ext(<<"vda">>)        -> <<"application/vda">>;
from_ext(<<"xlm">>)        -> <<"application/vnd.ms-excel">>;
from_ext(<<"cod">>)        -> <<"application/vnd.rim.cod">>;
from_ext(<<"pgn">>)        -> <<"application/x-chess-pgn">>;
from_ext(<<"cpio">>)       -> <<"application/x-cpio">>;
from_ext(<<"csh">>)        -> <<"application/x-csh">>;
from_ext(<<"deb">>)        -> <<"application/x-debian-package">>;
from_ext(<<"dcr">>)        -> <<"application/x-director">>;
from_ext(<<"dir">>)        -> <<"application/x-director">>;
from_ext(<<"dxr">>)        -> <<"application/x-director">>;
from_ext(<<"gz">>)         -> <<"application/x-gzip">>;
from_ext(<<"hdf">>)        -> <<"application/x-hdf">>;
from_ext(<<"ipx">>)        -> <<"application/x-ipix">>;
from_ext(<<"ips">>)        -> <<"application/x-ipscript">>;
from_ext(<<"js">>)         -> <<"application/x-javascript">>;
from_ext(<<"skd">>)        -> <<"application/x-koan">>;
from_ext(<<"skm">>)        -> <<"application/x-koan">>;
from_ext(<<"skp">>)        -> <<"application/x-koan">>;
from_ext(<<"skt">>)        -> <<"application/x-koan">>;
from_ext(<<"latex">>)      -> <<"application/x-latex">>;
from_ext(<<"lsp">>)        -> <<"application/x-lisp">>;
from_ext(<<"scm">>)        -> <<"application/x-lotusscreencam">>;
from_ext(<<"mif">>)        -> <<"application/x-mif">>;
from_ext(<<"com">>)        -> <<"application/x-msdos-program">>;
from_ext(<<"exe">>)        -> <<"application/octet-stream">>;
from_ext(<<"cdf">>)        -> <<"application/x-netcdf">>;
from_ext(<<"nc">>)         -> <<"application/x-netcdf">>;
from_ext(<<"pl">>)         -> <<"application/x-perl">>;
from_ext(<<"pm">>)         -> <<"application/x-perl">>;
from_ext(<<"rar">>)        -> <<"application/x-rar-compressed">>;
from_ext(<<"sh">>)         -> <<"application/x-sh">>;
from_ext(<<"shar">>)       -> <<"application/x-shar">>;
from_ext(<<"swf">>)        -> <<"application/x-shockwave-flash">>;
from_ext(<<"sit">>)        -> <<"application/x-stuffit">>;
from_ext(<<"sv4cpio">>)    -> <<"application/x-sv4cpio">>;
from_ext(<<"sv4crc">>)     -> <<"application/x-sv4crc">>;
from_ext(<<"tar.gz">>)     -> <<"application/x-tar-gz">>;
from_ext(<<"tgz">>)        -> <<"application/x-tar-gz">>;
from_ext(<<"tar">>)        -> <<"application/x-tar">>;
from_ext(<<"tcl">>)        -> <<"application/x-tcl">>;
from_ext(<<"texi">>)       -> <<"application/x-texinfo">>;
from_ext(<<"texinfo">>)    -> <<"application/x-texinfo">>;
from_ext(<<"man">>)        -> <<"application/x-troff-man">>;
from_ext(<<"me">>)         -> <<"application/x-troff-me">>;
from_ext(<<"ms">>)         -> <<"application/x-troff-ms">>;
from_ext(<<"roff">>)       -> <<"application/x-troff">>;
from_ext(<<"t">>)          -> <<"application/x-troff">>;
from_ext(<<"tr">>)         -> <<"application/x-troff">>;
from_ext(<<"ustar">>)      -> <<"application/x-ustar">>;
from_ext(<<"src">>)        -> <<"application/x-wais-source">>;
from_ext(<<"zip">>)        -> <<"application/zip">>;
from_ext(<<"tsi">>)        -> <<"audio/TSP-audio">>;
from_ext(<<"au">>)         -> <<"audio/basic">>;
from_ext(<<"snd">>)        -> <<"audio/basic">>;
from_ext(<<"kar">>)        -> <<"audio/midi">>;
from_ext(<<"mid">>)        -> <<"audio/midi">>;
from_ext(<<"midi">>)       -> <<"audio/midi">>;
from_ext(<<"mp2">>)        -> <<"audio/mpeg">>;
from_ext(<<"mp3">>)        -> <<"audio/mpeg">>;
from_ext(<<"mpga">>)       -> <<"audio/mpeg">>;
from_ext(<<"aif">>)        -> <<"audio/x-aiff">>;
from_ext(<<"aifc">>)       -> <<"audio/x-aiff">>;
from_ext(<<"aiff">>)       -> <<"audio/x-aiff">>;
from_ext(<<"m3u">>)        -> <<"audio/x-mpegurl">>;
from_ext(<<"wax">>)        -> <<"audio/x-ms-wax">>;
from_ext(<<"wma">>)        -> <<"audio/x-ms-wma">>;
from_ext(<<"rpm">>)        -> <<"audio/x-pn-realaudio-plugin">>;
from_ext(<<"ram">>)        -> <<"audio/x-pn-realaudio">>;
from_ext(<<"rm">>)         -> <<"audio/x-pn-realaudio">>;
from_ext(<<"ra">>)         -> <<"audio/x-realaudio">>;
from_ext(<<"wav">>)        -> <<"audio/x-wav">>;
from_ext(<<"pdb">>)        -> <<"chemical/x-pdb">>;
from_ext(<<"ras">>)        -> <<"image/cmu-raster">>;
from_ext(<<"gif">>)        -> <<"image/gif">>;
from_ext(<<"ief">>)        -> <<"image/ief">>;
from_ext(<<"jpe">>)        -> <<"image/jpeg">>;
from_ext(<<"jpeg">>)       -> <<"image/jpeg">>;
from_ext(<<"jpg">>)        -> <<"image/jpeg">>;
from_ext(<<"jp2">>)        -> <<"image/jp2">>;
from_ext(<<"png">>)        -> <<"image/png">>;
from_ext(<<"tif">>)        -> <<"image/tiff">>;
from_ext(<<"tiff">>)       -> <<"image/tiff">>;
from_ext(<<"pnm">>)        -> <<"image/x-portable-anymap">>;
from_ext(<<"pbm">>)        -> <<"image/x-portable-bitmap">>;
from_ext(<<"pgm">>)        -> <<"image/x-portable-graymap">>;
from_ext(<<"ppm">>)        -> <<"image/x-portable-pixmap">>;
from_ext(<<"rgb">>)        -> <<"image/x-rgb">>;
from_ext(<<"xbm">>)        -> <<"image/x-xbitmap">>;
from_ext(<<"xwd">>)        -> <<"image/x-xwindowdump">>;
from_ext(<<"iges">>)       -> <<"model/iges">>;
from_ext(<<"igs">>)        -> <<"model/iges">>;
from_ext(<<"mesh">>)       -> <<"model/mesh">>;
from_ext(<<"msh">>)        -> <<"model/mesh">>;
from_ext(<<"silo">>)       -> <<"model/mesh">>;
from_ext(<<"vrml">>)       -> <<"model/vrml">>;
from_ext(<<"wrl">>)        -> <<"model/vrml">>;
from_ext(<<"css">>)        -> <<"text/css">>;
from_ext(<<"htm">>)        -> <<"text/html">>;
from_ext(<<"html">>)       -> <<"text/html">>;
from_ext(<<"asc">>)        -> <<"text/plain">>;
from_ext(<<"c">>)          -> <<"text/plain">>;
from_ext(<<"cc">>)         -> <<"text/plain">>;
from_ext(<<"f90">>)        -> <<"text/plain">>;
from_ext(<<"f">>)          -> <<"text/plain">>;
from_ext(<<"hh">>)         -> <<"text/plain">>;
from_ext(<<"m">>)          -> <<"text/plain">>;
from_ext(<<"txt">>)        -> <<"text/plain">>;
from_ext(<<"rtx">>)        -> <<"text/richtext">>;
from_ext(<<"sgm">>)        -> <<"text/sgml">>;
from_ext(<<"sgml">>)       -> <<"text/sgml">>;
from_ext(<<"tsv">>)        -> <<"text/tab-separated-values">>;
from_ext(<<"jad">>)        -> <<"text/vnd.sun.j2me.app-descriptor">>;
from_ext(<<"etx">>)        -> <<"text/x-setext">>;
from_ext(<<"xml">>)        -> <<"application/xml">>;
from_ext(<<"dl">>)         -> <<"video/dl">>;
from_ext(<<"fli">>)        -> <<"video/fli">>;
from_ext(<<"flv">>)        -> <<"video/x-flv">>;
from_ext(<<"gl">>)         -> <<"video/gl">>;
from_ext(<<"mp4">>)        -> <<"video/mp4">>;
from_ext(<<"mpe">>)        -> <<"video/mpeg">>;
from_ext(<<"mpeg">>)       -> <<"video/mpeg">>;
from_ext(<<"mpg">>)        -> <<"video/mpeg">>;
from_ext(<<"mov">>)        -> <<"video/quicktime">>;
from_ext(<<"qt">>)         -> <<"video/quicktime">>;
from_ext(<<"viv">>)        -> <<"video/vnd.vivo">>;
from_ext(<<"vivo">>)       -> <<"video/vnd.vivo">>;
from_ext(<<"asf">>)        -> <<"video/x-ms-asf">>;
from_ext(<<"asx">>)        -> <<"video/x-ms-asx">>;
from_ext(<<"wmv">>)        -> <<"video/x-ms-wmv">>;
from_ext(<<"wmx">>)        -> <<"video/x-ms-wmx">>;
from_ext(<<"wvx">>)        -> <<"video/x-ms-wvx">>;
from_ext(<<"avi">>)        -> <<"video/x-msvideo">>;
from_ext(<<"movie">>)      -> <<"video/x-sgi-movie">>;
from_ext(<<"mime">>)       -> <<"www/mime">>;
from_ext(<<"ice">>)        -> <<"x-conference/x-cooltalk">>;
from_ext(<<"vrm">>)        -> <<"x-world/x-vrml">>;
from_ext(<<"spx">>)        -> <<"audio/ogg">>;
from_ext(<<"xhtml">>)      -> <<"application/xhtml+xml">>;
from_ext(<<"bz2">>)        -> <<"application/x-bzip2">>;
from_ext(<<"doc">>)        -> <<"application/msword">>;
from_ext(<<"z">>)          -> <<"application/x-compress">>;
from_ext(<<"ico">>)        -> <<"image/x-icon">>;
from_ext(<<"bmp">>)        -> <<"image/bmp">>;
from_ext(<<"m4a">>)        -> <<"audio/mpeg">>;
from_ext(<<"csv">>)        -> <<"text/csv">>;
from_ext(_)                 -> undefined.

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------
-ifdef(TEST).
-include_lib(<<"eunit/include/eunit.hrl">>).

from_ext_test() ->
    ?assertEqual(<<"image/x-ico">>,
                 from_ext(<<"ico">>)),
    ?assertEqual(<<"text/html">>,
                 from_ext(<<"html">>)),
    ?assertEqual(undefined,
                 from_ext(<<"">>)),
    ?assertEqual(undefined,
                 from_ext(<<"wtf">>)),
    ok.

-endif.

