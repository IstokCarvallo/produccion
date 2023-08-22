$PBExportHeader$w_informes_con_pregunta.srw
forward
global type w_informes_con_pregunta from window
end type
type pb_correo from picturebutton within w_informes_con_pregunta
end type
type pb_ordenar from picturebutton within w_informes_con_pregunta
end type
type pb_ajuste from picturebutton within w_informes_con_pregunta
end type
type em_zoom from editmask within w_informes_con_pregunta
end type
type pb_genarc from picturebutton within w_informes_con_pregunta
end type
type pb_regla from picturebutton within w_informes_con_pregunta
end type
type pb_next from picturebutton within w_informes_con_pregunta
end type
type pb_prior from picturebutton within w_informes_con_pregunta
end type
type pb_first from picturebutton within w_informes_con_pregunta
end type
type pb_last from picturebutton within w_informes_con_pregunta
end type
type pb_imprimir from picturebutton within w_informes_con_pregunta
end type
type pb_salir from picturebutton within w_informes_con_pregunta
end type
type dw_1 from datawindow within w_informes_con_pregunta
end type
type gb_4 from groupbox within w_informes_con_pregunta
end type
type gb_1 from groupbox within w_informes_con_pregunta
end type
type gb_2 from groupbox within w_informes_con_pregunta
end type
type gb_3 from groupbox within w_informes_con_pregunta
end type
type gb_5 from groupbox within w_informes_con_pregunta
end type
type gb_6 from groupbox within w_informes_con_pregunta
end type
type gb_7 from groupbox within w_informes_con_pregunta
end type
type wstr_ano_benef from structure within w_informes_con_pregunta
end type
end forward

type wstr_ano_benef from structure
    integer ano
    integer beneficio
end type

shared variables

end variables

global type w_informes_con_pregunta from window
boolean visible = false
integer width = 3625
integer height = 2288
boolean titlebar = true
boolean controlmenu = true
boolean minbox = true
windowtype windowtype = popup!
long backcolor = 12632256
string icon = "RunReport5!"
pb_correo pb_correo
pb_ordenar pb_ordenar
pb_ajuste pb_ajuste
em_zoom em_zoom
pb_genarc pb_genarc
pb_regla pb_regla
pb_next pb_next
pb_prior pb_prior
pb_first pb_first
pb_last pb_last
pb_imprimir pb_imprimir
pb_salir pb_salir
dw_1 dw_1
gb_4 gb_4
gb_1 gb_1
gb_2 gb_2
gb_3 gb_3
gb_5 gb_5
gb_6 gb_6
gb_7 gb_7
end type
global w_informes_con_pregunta w_informes_con_pregunta

type variables
str_info istr_info
end variables

forward prototypes
public function integer wf_rollo (integer ai_rollo, integer ai_fin)
public function integer wf_nuevo_rollo (integer ai_rollo, integer ai_fin)
end prototypes

public function integer wf_rollo (integer ai_rollo, integer ai_fin);long ll_bloque, ll_fila
int i
ll_bloque = dw_1.rowcount() + 1
for i = 1 to ai_fin
	ll_fila = dw_1.insertrow(0)
	dw_1.setitem(ll_fila,"rollo",ai_rollo)
	dw_1.setitem(ll_fila,"tam",dw_1.getitemstring(i,"tam"))
	dw_1.setitem(ll_fila,"rut",dw_1.getitemnumber(i,"rut"))
	dw_1.setitem(ll_fila,"fon",dw_1.getitemstring(i,"fon"))
	dw_1.setitem(ll_fila,"mon",dw_1.getitemnumber(i,"mon"))
	dw_1.setitem(ll_fila,"mon_ant",dw_1.getitemnumber(i,"mon_ant"))
	dw_1.setitem(ll_fila,"porc",dw_1.getitemdecimal(i,"porc"))
	dw_1.setitem(ll_fila,"var_ant",dw_1.getitemdecimal(i,"var_ant"))
	dw_1.setitem(ll_fila,"var_dic",dw_1.getitemdecimal(i,"var_dic"))
	dw_1.setitem(ll_fila,"plazo",dw_1.getitemdecimal(i,"plazo"))
	dw_1.setitem(ll_fila,"plazo_ant",dw_1.getitemdecimal(i,"plazo_ant"))
next
return ll_bloque
end function

public function integer wf_nuevo_rollo (integer ai_rollo, integer ai_fin);long ll_bloque, ll_fila
int i
ll_bloque = dw_1.rowcount() + 1
for i = 1 to ai_fin
	ll_fila = dw_1.insertrow(0)
	dw_1.setitem(ll_fila,"roll",ai_rollo)
	dw_1.setitem(ll_fila,"tam",dw_1.getitemstring(i,"tam"))
	dw_1.setitem(ll_fila,"rut",dw_1.getitemnumber(i,"rut"))
	dw_1.setitem(ll_fila,"fon",dw_1.getitemstring(i,"fon"))
	dw_1.setitem(ll_fila,"mon",dw_1.getitemnumber(i,"mon"))
	dw_1.setitem(ll_fila,"porc",dw_1.getitemdecimal(i,"porc"))
next
return ll_bloque
end function

event open;This.Icon	= Mid(gstr_apl.bmp,1,Pos(gstr_apl.bmp,'.'))+'ico'

istr_info	=	Message.PowerObjectParm
This.Title	=	istr_info.titulo

IF Isnull(istr_info.orden) OR istr_info.orden = 0 THEN
	pb_ordenar.Visible	=	False
	//gb_7.Visible			=	False
END IF

IF IsNull(istr_info.Multiple) THEN istr_info.Multiple = False

F_Membrete(dw_1)

pb_imprimir.SetFocus()
end event

on w_informes_con_pregunta.create
this.pb_correo=create pb_correo
this.pb_ordenar=create pb_ordenar
this.pb_ajuste=create pb_ajuste
this.em_zoom=create em_zoom
this.pb_genarc=create pb_genarc
this.pb_regla=create pb_regla
this.pb_next=create pb_next
this.pb_prior=create pb_prior
this.pb_first=create pb_first
this.pb_last=create pb_last
this.pb_imprimir=create pb_imprimir
this.pb_salir=create pb_salir
this.dw_1=create dw_1
this.gb_4=create gb_4
this.gb_1=create gb_1
this.gb_2=create gb_2
this.gb_3=create gb_3
this.gb_5=create gb_5
this.gb_6=create gb_6
this.gb_7=create gb_7
this.Control[]={this.pb_correo,&
this.pb_ordenar,&
this.pb_ajuste,&
this.em_zoom,&
this.pb_genarc,&
this.pb_regla,&
this.pb_next,&
this.pb_prior,&
this.pb_first,&
this.pb_last,&
this.pb_imprimir,&
this.pb_salir,&
this.dw_1,&
this.gb_4,&
this.gb_1,&
this.gb_2,&
this.gb_3,&
this.gb_5,&
this.gb_6,&
this.gb_7}
end on

on w_informes_con_pregunta.destroy
destroy(this.pb_correo)
destroy(this.pb_ordenar)
destroy(this.pb_ajuste)
destroy(this.em_zoom)
destroy(this.pb_genarc)
destroy(this.pb_regla)
destroy(this.pb_next)
destroy(this.pb_prior)
destroy(this.pb_first)
destroy(this.pb_last)
destroy(this.pb_imprimir)
destroy(this.pb_salir)
destroy(this.dw_1)
destroy(this.gb_4)
destroy(this.gb_1)
destroy(this.gb_2)
destroy(this.gb_3)
destroy(this.gb_5)
destroy(this.gb_6)
destroy(this.gb_7)
end on

event resize;dw_1.Width		=	This.Width - 375
dw_1.Height		=	This.Height - 176

gb_1.x 			=	This.Width - 288
pb_first.x 		=	This.Width - 234
pb_prior.x 		=	This.Width - 234
pb_next.x 		=	This.Width - 234
pb_last.x 		=	This.Width - 234

gb_2.x 			=	This.Width - 288
pb_regla.x 		=	This.Width - 234

gb_3.x 			=	This.Width - 288
pb_imprimir.x 	=	This.Width - 234

gb_4.x 			=	This.Width - 288
pb_salir.x 		=	This.Width - 234

gb_5.x 			=	This.Width - 288
pb_genarc.x 	=	This.Width - 234

gb_6.x 			=	This.Width - 288
pb_ajuste.x	 	=	This.Width - 234

gb_7.x 			=	This.Width - 288
pb_correo.x	 	=	This.Width - 234

em_zoom.x 	=	This.Width - 288
end event

type pb_correo from picturebutton within w_informes_con_pregunta
event mousemove pbm_mousemove
string tag = "Ordenamiento de Informe"
integer x = 3419
integer y = 888
integer width = 114
integer height = 108
integer taborder = 90
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo\Bmp\correoe.bmp"
string disabledname = "\Desarrollo\Bmp\correod.bmp"
alignment htextalign = left!
string powertiptext = "Ordenamiento de Informe"
end type

event mousemove;RETURN w_main.SetMicroHelp(This.Tag)
end event

event clicked;String		ls_Nombre, ls_NomReporte, ls_Archivo, ls_DirectorioAct 
Long			ll_Fila, ll_consignatario, ll_Archivo
Boolean		lb_Existe
Integer		li_imprimio
str_parms	lstr_parms

SetPointer(HourGlass!)

ls_DirectorioAct=GetCurrentDirectory()
	
ls_Archivo									=	ls_DirectorioAct + '\Reporte' + &
													String(today(),'yyyymmdd')+'.pdf'
lstr_parms.string_arg[ll_Archivo+3]	=	ls_Archivo

dw_1.SaveAs(ls_Archivo, PDF!  , True)

ChangeDirectory ( ls_DirectorioAct )

ll_Archivo	=	1 
ls_NomReporte					=	'Reportes' + String(today(),'yyyymmdd')
lstr_parms.string_arg[1]	=	String(1)
lstr_parms.string_arg[2]	=	ls_NomReporte
lstr_parms.string_arg[3]	=	String(1)

lstr_parms.string_arg[ll_Archivo+3]	=	ls_Archivo

ChangeDirectory(ls_DirectorioAct)

OpenWithParm(w_correo, lstr_parms)

DO WHILE ll_Archivo>0 
	lb_Existe	=	FileExists(lstr_parms.string_arg[ll_Archivo + 3])
	
	IF lb_Existe THEN
		FileDelete(lstr_parms.string_arg[ll_Archivo + 3])
	END IF
	
	ll_Archivo --
LOOP

ChangeDirectory ( ls_DirectorioAct )

SetPointer(Arrow!)
end event

type pb_ordenar from picturebutton within w_informes_con_pregunta
event mousemove pbm_mousemove
string tag = "Ordenamiento de Informe"
integer x = 3419
integer y = 888
integer width = 114
integer height = 108
integer taborder = 60
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo\Bmp\Distribe.bmp"
alignment htextalign = left!
string powertiptext = "Ordenamiento de Informe"
end type

event mousemove;RETURN w_main.SetMicroHelp(This.Tag)
end event

event clicked;String ls_info

str_parms	parm

parm.string_arg[1]	= ""
parm.dw_arg				= dw_1

OpenWithParm(w_columna_orden, parm)

ls_info	= Message.StringParm

RETURN
end event

type pb_ajuste from picturebutton within w_informes_con_pregunta
event mousemove pbm_mousemove
string tag = "Ajuste de Impresión en página"
integer x = 3419
integer y = 1112
integer width = 114
integer height = 108
integer taborder = 80
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo\Bmp\Buscae.bmp"
alignment htextalign = left!
string powertiptext = "Ajuste de Página"
end type

event mousemove;RETURN w_main.SetMicroHelp(This.Tag)
end event

event type long clicked(integer m);String	ls_zoom
Integer	li_zoom
str_zoom	lstr_zoom

lstr_zoom.idw_obj = dw_1

ls_zoom	=	dw_1.object.datawindow.zoom

IF IsNumber(ls_zoom) THEN
	lstr_zoom.zoom = Integer(ls_zoom)
END IF

OpenWithParm (w_zoom, lstr_zoom)

li_zoom = message.DoubleParm

RETURN 0


end event

type em_zoom from editmask within w_informes_con_pregunta
event spun pbm_enchange
string tag = "Porcentaje de ampliación"
integer x = 3365
integer y = 720
integer width = 224
integer height = 92
integer taborder = 70
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "75"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "###"
boolean spin = true
double increment = 25
string minmax = "0~~200"
end type

event type long spun();IF Len(This.Text) > 0 THEN
	IF Integer(This.Text) > 25 THEN This.TriggerEvent(modified!)
END IF

RETURN 0
end event

event modified;SetPointer(HourGlass!)

dw_1.Modify('DataWindow.print.preview.Zoom = ' + This.Text)

SetPointer(Arrow!)
end event

type pb_genarc from picturebutton within w_informes_con_pregunta
event mousemove pbm_mousemove
string tag = "Genera a Archivo"
integer x = 3419
integer y = 1784
integer width = 114
integer height = 108
integer taborder = 90
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo\Bmp\BuscaArc.bmp"
string disabledname = "\Desarrollo\Bmp\BuscaArc.bmp"
string powertiptext = "Genera a Archivo"
end type

event mousemove;RETURN w_main.SetMicroHelp(This.Tag)
end event

event clicked;IF dw_1.SaveAs() = 1 THEN
	MessageBox("Generación de Archivo","Se ha generado el Archivo")
	RETURN
ELSE
	MessageBox("Atención","Ocurrió un error al generar el Archivo.~r~n"+&
					"Revise permisos sobre carpeta o espacio en disco.")
	RETURN
END IF

end event

type pb_regla from picturebutton within w_informes_con_pregunta
event mousemove pbm_mousemove
string tag = "Incorpora / Elimina Regleta"
integer x = 3419
integer y = 1336
integer width = 114
integer height = 108
integer taborder = 70
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\reglae.bmp"
string disabledname = "\desarrollo\bmp\reglad.bmp"
alignment htextalign = left!
string powertiptext = "Regleta"
end type

event type long mousemove(unsignedlong flags, integer xpos, integer ypos);RETURN w_main.SetMicroHelp(This.Tag)
end event

event clicked;SetPointer(HourGlass!)

IF dw_1.Describe("datawindow.print.preview.rulers") = "no" THEN
	dw_1.Modify ("datawindow.print.preview.rulers = yes")
ELSE
	dw_1.Modify ("datawindow.print.preview.rulers = no")
END IF
end event

type pb_next from picturebutton within w_informes_con_pregunta
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Página Siguiente"
integer x = 3419
integer y = 388
integer width = 114
integer height = 108
integer taborder = 40
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\nexte.bmp"
string disabledname = "\desarrollo\bmp\nextd.bmp"
alignment htextalign = left!
string powertiptext = "Página Siguiente"
end type

event clicked;dw_1.ScrollNextPage()

pb_first.Enabled	= True
pb_prior.Enabled	= True

IF Long(dw_1.Object.DataWindow.LastRowOnPage) = dw_1.RowCount() THEN
	pb_next.Enabled	= False
	pb_last.Enabled	= False
END IF
end event

event mousemove;RETURN w_main.SetMicroHelp(This.Tag)
end event

type pb_prior from picturebutton within w_informes_con_pregunta
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Página Anterior"
integer x = 3419
integer y = 236
integer width = 114
integer height = 108
integer taborder = 30
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\desarrollo\bmp\priore.bmp"
string disabledname = "\desarrollo\bmp\priord.bmp"
alignment htextalign = left!
string powertiptext = "Página Anterior"
end type

event clicked;dw_1.ScrollPriorPage()

pb_next.Enabled	= True
pb_last.Enabled	= True

IF Long(dw_1.Object.DataWindow.FirstRowOnPage) = 1 THEN
	pb_first.Enabled	= False
	pb_prior.Enabled	= False
END IF
end event

event mousemove;RETURN w_main.SetMicroHelp(This.Tag)
end event

type pb_first from picturebutton within w_informes_con_pregunta
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Inicio del Reporte"
integer x = 3419
integer y = 84
integer width = 114
integer height = 108
integer taborder = 20
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\desarrollo\bmp\firste.bmp"
string disabledname = "\desarrollo\bmp\firstd.bmp"
alignment htextalign = left!
string powertiptext = "Inicio del Reporte"
end type

event clicked;dw_1.ScrollToRow(0)

pb_first.Enabled	= False
pb_prior.Enabled	= False
pb_next.Enabled	= True
pb_last.Enabled	= True
end event

event mousemove;RETURN w_main.SetMicroHelp(This.Tag)
end event

type pb_last from picturebutton within w_informes_con_pregunta
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Final del Reporte"
integer x = 3419
integer y = 540
integer width = 114
integer height = 108
integer taborder = 50
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\laste.bmp"
string disabledname = "\desarrollo\bmp\lastd.bmp"
alignment htextalign = left!
string powertiptext = "Final del Reporte"
end type

event clicked;dw_1.ScrollToRow(9999999)

pb_first.Enabled	= True
pb_prior.Enabled	= True
pb_next.Enabled	= False
pb_last.Enabled	= False
end event

event mousemove;RETURN w_main.SetMicroHelp(This.Tag)
end event

type pb_imprimir from picturebutton within w_informes_con_pregunta
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Imprimir Informe a Impresora"
integer x = 3419
integer y = 1560
integer width = 114
integer height = 108
integer taborder = 80
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\desarrollo\bmp\imprimee.bmp"
string disabledname = "\desarrollo\bmp\imprimed.bmp"
string powertiptext = "Imprimir Informe a Impresora"
end type

event clicked;SetPointer(HourGlass!)
Integer	li_imprimio

istr_info.dw = dw_1

OpenWithParm(w_opc_impresion,istr_info)

IF Message.DoubleParm = 1 THEN
	li_imprimio = dw_1.Print( )
	
	IF li_imprimio = 1 AND IsValid(w_elimina_existencia_packing) THEN
		w_elimina_existencia_packing.Elimina_Existencia()
	END IF
	
ELSE
	li_imprimio = -1
END IF

SetPointer(Arrow!)
end event

event type long mousemove(unsignedlong flags, integer xpos, integer ypos);RETURN w_main.SetMicroHelp(This.Tag)
end event

type pb_salir from picturebutton within w_informes_con_pregunta
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Salir"
integer x = 3419
integer y = 2008
integer width = 114
integer height = 108
integer taborder = 100
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\desarrollo\bmp\exit.bmp"
string disabledname = "\desarrollo\bmp\exitd.bmp"
alignment htextalign = left!
string powertiptext = "Salir"
end type

on clicked;Close(Parent)
end on

event mousemove;RETURN w_main.SetMicroHelp(This.Tag)
end event

type dw_1 from datawindow within w_informes_con_pregunta
integer x = 37
integer y = 32
integer width = 3278
integer height = 2120
integer taborder = 10
boolean hscrollbar = true
boolean vscrollbar = true
string icon = "WinLogo!"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event retrieveend;IF Long(dw_1.Object.DataWindow.LastRowOnPage) < dw_1.RowCount() THEN
	pb_next.Enabled	= True
	pb_last.Enabled	= True
ELSE
	pb_next.Enabled	= False
	pb_last.Enabled	= False
END IF
end event

event retrievestart;IF istr_info.Multiple THEN RETURN 2
end event

type gb_4 from groupbox within w_informes_con_pregunta
integer x = 3365
integer y = 1264
integer width = 224
integer height = 212
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type gb_1 from groupbox within w_informes_con_pregunta
integer x = 3365
integer width = 224
integer height = 696
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type gb_2 from groupbox within w_informes_con_pregunta
integer x = 3365
integer y = 1488
integer width = 224
integer height = 212
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type gb_3 from groupbox within w_informes_con_pregunta
integer x = 3365
integer y = 1936
integer width = 224
integer height = 212
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type gb_5 from groupbox within w_informes_con_pregunta
integer x = 3365
integer y = 1712
integer width = 224
integer height = 212
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type gb_6 from groupbox within w_informes_con_pregunta
integer x = 3365
integer y = 1040
integer width = 224
integer height = 212
integer taborder = 80
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type gb_7 from groupbox within w_informes_con_pregunta
integer x = 3365
integer y = 816
integer width = 224
integer height = 212
integer taborder = 90
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

