$PBExportHeader$w_gene_archivo_cabrini.srw
$PBExportComments$Genera archivo Plano SAG por Recepciones de Pallets Inter Planta.
forward
global type w_gene_archivo_cabrini from window
end type
type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_cabrini
end type
type uo_selplantas from uo_seleccion_plantas within w_gene_archivo_cabrini
end type
type em_desde from editmask within w_gene_archivo_cabrini
end type
type st_2 from statictext within w_gene_archivo_cabrini
end type
type st_numero from statictext within w_gene_archivo_cabrini
end type
type st_6 from statictext within w_gene_archivo_cabrini
end type
type st_4 from statictext within w_gene_archivo_cabrini
end type
type st_3 from statictext within w_gene_archivo_cabrini
end type
type sle_mensa from singlelineedit within w_gene_archivo_cabrini
end type
type em_hasta from editmask within w_gene_archivo_cabrini
end type
type dw_1 from datawindow within w_gene_archivo_cabrini
end type
type st_5 from statictext within w_gene_archivo_cabrini
end type
type st_1 from statictext within w_gene_archivo_cabrini
end type
type pb_salir from picturebutton within w_gene_archivo_cabrini
end type
type pb_grabar from picturebutton within w_gene_archivo_cabrini
end type
end forward

global type w_gene_archivo_cabrini from window
integer width = 2542
integer height = 1252
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
string icon = "TABLA.ICO"
event ue_guardar ( )
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
uo_selcliente uo_selcliente
uo_selplantas uo_selplantas
em_desde em_desde
st_2 st_2
st_numero st_numero
st_6 st_6
st_4 st_4
st_3 st_3
sle_mensa sle_mensa
em_hasta em_hasta
dw_1 dw_1
st_5 st_5
st_1 st_1
pb_salir pb_salir
pb_grabar pb_grabar
end type
global w_gene_archivo_cabrini w_gene_archivo_cabrini

type variables
Boolean			ib_Anulacion

Date				id_FechaRepa, id_FechaAcceso
Time				it_HoraAcceso
end variables

event ue_guardar();Long			ll_Fila, ll_Filas, ll_FilaDet, ll_cancaj, ll_pallet
String			ls_Archivo, ls_Registro, ls_NumeroSAG, ls_embalaje, ls_tipopa
Integer		li_cliente, li_planta, li_etiqueta, li_Etiq
Date			ld_fe2

dw_1.reset()
dw_1.SetTransObject(Sqlca)

ib_Anulacion	=	True

IF ib_Anulacion THEN
	ll_Filas		= dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlantas.Codigo, Date(em_Desde.Text), Date(em_Hasta.Text))
	
	IF ll_Filas = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Pallets")
	ELSEIF ll_Filas = 0 THEN
		MessageBox("Atención", "No registra Información según parámetros.", Exclamation!, Ok!)

	ELSE
		ls_Archivo	=	"CABRINI" +&
							String(Year(Today())) + &
							String(Month(Today()),'00') + &
							String(Day(Today()),'00') + ".XLS"
		IF dw_1.SaveAs(gs_disco+":\GeneradosSAAM\" + ls_Archivo, Excel! , True) = -1 THEN
			MessageBox("Atención","No se pudo generar el archivo " + ls_Archivo)
		ELSE
			sle_mensa.text	= "Archivo " + ls_Archivo + " Generado. Avise a Computación."
		END IF
	END IF
END IF


end event

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

on w_gene_archivo_cabrini.create
this.uo_selcliente=create uo_selcliente
this.uo_selplantas=create uo_selplantas
this.em_desde=create em_desde
this.st_2=create st_2
this.st_numero=create st_numero
this.st_6=create st_6
this.st_4=create st_4
this.st_3=create st_3
this.sle_mensa=create sle_mensa
this.em_hasta=create em_hasta
this.dw_1=create dw_1
this.st_5=create st_5
this.st_1=create st_1
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.Control[]={this.uo_selcliente,&
this.uo_selplantas,&
this.em_desde,&
this.st_2,&
this.st_numero,&
this.st_6,&
this.st_4,&
this.st_3,&
this.sle_mensa,&
this.em_hasta,&
this.dw_1,&
this.st_5,&
this.st_1,&
this.pb_salir,&
this.pb_grabar}
end on

on w_gene_archivo_cabrini.destroy
destroy(this.uo_selcliente)
destroy(this.uo_selplantas)
destroy(this.em_desde)
destroy(this.st_2)
destroy(this.st_numero)
destroy(this.st_6)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.sle_mensa)
destroy(this.em_hasta)
destroy(this.dw_1)
destroy(this.st_5)
destroy(this.st_1)
destroy(this.pb_salir)
destroy(this.pb_grabar)
end on

event open;x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

uo_SelCliente.Seleccion(False, False)
uo_SelPlantas.Seleccion(False, False)
uo_SelPlantas.Inicia(gi_CodPlanta)
uo_SelCliente.Inicia(gi_codexport)

em_Desde.Text		=	String(Today())
em_Hasta.Text		=	String(Today())

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)

end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_cabrini
event destroy ( )
integer x = 750
integer y = 284
integer height = 100
integer taborder = 40
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplantas from uo_seleccion_plantas within w_gene_archivo_cabrini
event destroy ( )
integer x = 750
integer y = 432
integer height = 100
integer taborder = 20
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

type em_desde from editmask within w_gene_archivo_cabrini
integer x = 750
integer y = 596
integer width = 434
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
string displaydata = "@"
end type

event modified;
IF Date(em_Hasta.Text) <  Date(This.Text) THEN
	MessageBox("Atención", "La Fecha Debe ser Menor o Igual a la Fecha Hasta.~r~r" + &
				  "Ingrese Otra.", Exclamation!, Ok!)
	This.Text					=	String(Today())
	This.SetFocus()
END IF


end event

type st_2 from statictext within w_gene_archivo_cabrini
integer x = 142
integer y = 744
integer width = 407
integer height = 84
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Hasta"
boolean focusrectangle = false
end type

type st_numero from statictext within w_gene_archivo_cabrini
integer x = 142
integer y = 596
integer width = 407
integer height = 84
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Desde"
boolean focusrectangle = false
end type

type st_6 from statictext within w_gene_archivo_cabrini
integer x = 82
integer y = 920
integer width = 1970
integer height = 224
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_4 from statictext within w_gene_archivo_cabrini
integer x = 142
integer y = 448
integer width = 407
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta "
boolean focusrectangle = false
end type

type st_3 from statictext within w_gene_archivo_cabrini
integer x = 142
integer y = 300
integer width = 407
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type sle_mensa from singlelineedit within w_gene_archivo_cabrini
integer x = 133
integer y = 972
integer width = 1870
integer height = 112
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
boolean italic = true
long textcolor = 65535
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type em_hasta from editmask within w_gene_archivo_cabrini
integer x = 750
integer y = 744
integer width = 434
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
string displaydata = "@"
end type

event modified;
IF Date(em_Desde.Text) >  Date(This.Text) THEN
	MessageBox("Atención", "La Fecha Debe ser Mayor o Igual a la Fecha Desde.~r~r" + &
				  "Ingrese Otra.", Exclamation!, Ok!)
	This.Text					=	em_Desde.Text
END IF


end event

type dw_1 from datawindow within w_gene_archivo_cabrini
boolean visible = false
integer x = 786
integer y = 1184
integer width = 1006
integer height = 400
string dataobject = "dw_info_procesosfacturacionclientes"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
end type

type st_5 from statictext within w_gene_archivo_cabrini
integer x = 78
integer y = 68
integer width = 1970
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
string text = "Genera Archivo Cabrini"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_1 from statictext within w_gene_archivo_cabrini
integer x = 82
integer y = 224
integer width = 1970
integer height = 692
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_gene_archivo_cabrini
integer x = 2162
integer y = 884
integer width = 302
integer height = 244
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_gene_archivo_cabrini
integer x = 2162
integer y = 592
integer width = 302
integer height = 244
integer taborder = 50
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_guardar")
end event

