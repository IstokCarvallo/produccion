$PBExportHeader$w_gene_archivo_recepciones_cabrini.srw
$PBExportComments$Genera archivo Plano SAG por Recepciones de Pallets Inter Planta.
forward
global type w_gene_archivo_recepciones_cabrini from window
end type
type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_recepciones_cabrini
end type
type dw_2 from datawindow within w_gene_archivo_recepciones_cabrini
end type
type em_desde from editmask within w_gene_archivo_recepciones_cabrini
end type
type st_2 from statictext within w_gene_archivo_recepciones_cabrini
end type
type st_numero from statictext within w_gene_archivo_recepciones_cabrini
end type
type st_6 from statictext within w_gene_archivo_recepciones_cabrini
end type
type st_3 from statictext within w_gene_archivo_recepciones_cabrini
end type
type sle_mensa from singlelineedit within w_gene_archivo_recepciones_cabrini
end type
type em_hasta from editmask within w_gene_archivo_recepciones_cabrini
end type
type dw_1 from datawindow within w_gene_archivo_recepciones_cabrini
end type
type st_5 from statictext within w_gene_archivo_recepciones_cabrini
end type
type pb_salir from picturebutton within w_gene_archivo_recepciones_cabrini
end type
type pb_grabar from picturebutton within w_gene_archivo_recepciones_cabrini
end type
type st_1 from statictext within w_gene_archivo_recepciones_cabrini
end type
end forward

global type w_gene_archivo_recepciones_cabrini from window
integer width = 2501
integer height = 1284
boolean titlebar = true
string title = "Genera Plano Recepciones Cabrini"
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
dw_2 dw_2
em_desde em_desde
st_2 st_2
st_numero st_numero
st_6 st_6
st_3 st_3
sle_mensa sle_mensa
em_hasta em_hasta
dw_1 dw_1
st_5 st_5
pb_salir pb_salir
pb_grabar pb_grabar
st_1 st_1
end type
global w_gene_archivo_recepciones_cabrini w_gene_archivo_recepciones_cabrini

type variables
Boolean			ib_Anulacion

Date				id_FechaRepa, id_FechaAcceso
Time				it_HoraAcceso


end variables

event ue_guardar();Long			ll_Fila, ll_Filas, ll_FilaDet, ll_Numero, ll_Guia, ll_Pallet, ll_Altura, &
				ll_Productor, ll_Cajas 
String			ls_Archivo, ls_Registro, ls_Embalaje, ls_Calibre
Integer		li_Packing, li_Especie, li_Etiqueta, li_Variedad
Date			ld_Fecrec, ld_Fecemb

dw_2.reset()
dw_1.reset()

dw_2.SetTransObject(Sqlca)

ib_Anulacion=	True


IF ib_Anulacion THEN
	ll_Filas		= dw_2.Retrieve(uo_SelCliente.Codigo, Date(em_Desde.Text), Date(em_Hasta.Text))
	
	IF ll_Filas = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Recepciones")
	ELSEIF ll_Filas = 0 THEN
		MessageBox("Atención", "No registra Información según parámetros.", Exclamation!, Ok!)
	ELSE
		dw_2.SetSort('rfpe_fecrec')
      	dw_2.Sort()
		
		ls_Archivo	=	"CABRINIPLANO" +&
							String(Year(Today())) + &
							String(Month(Today()),'00') + &
							String(Day(Today()),'00') + ".txt"
		
		FOR ll_Fila = 1 TO ll_Filas
			ld_Fecrec	=  dw_2.Object.rfpe_fecrec[ll_Fila]
			ll_Numero	=	dw_2.Object.rfpe_numero[ll_Fila]			
			ll_Guia		= 	dw_2.Object.rfpe_nrores[ll_Fila]
			ll_Pallet	=	dw_2.Object.paen_numero[ll_Fila]
			li_Packing	=	dw_2.Object.rfpe_ptaori[ll_Fila]
			ls_Embalaje	=	dw_2.Object.emba_codigo[ll_Fila]
			ll_Altura	=	dw_2.Object.altura[ll_Fila]			
			ld_Fecemb	=	dw_2.Object.paen_fecemb[ll_Fila]
			ll_Productor=	dw_2.Object.prod_codigo[ll_Fila]
			li_Especie	=	dw_2.Object.espe_codigo[ll_Fila]
			li_Etiqueta	=	dw_2.Object.etiq_codigo[ll_Fila]
			li_Variedad	=	dw_2.Object.vari_codigo[ll_Fila]
			ls_Calibre	=	dw_2.Object.pafr_calibr[ll_Fila]			
			ll_Cajas		=	dw_2.Object.pafr_ccajas[ll_Fila]
	
			ls_Registro	 =	String(Day(ld_Fecrec),'00')
			ls_Registro	+=	'-'
			ls_Registro	+=	String(Month(ld_Fecrec),'00')
			ls_Registro	+=	'-'
			ls_Registro	+=	String(Year(ld_Fecrec)) 
			ls_Registro	+=	String(ll_Numero, '00000000')
			ls_Registro	+=	String(ll_Guia, '00000000')			
			ls_Registro	+=	String(ll_Pallet, '0000000000')	
			ls_Registro	+=	String(li_Packing, '0000')
			ls_Registro	+=	ls_Embalaje + Fill(' ',4 - len(ls_Embalaje))
			ls_Registro	+=	String(ll_Altura, '0000')
			ls_Registro	+=	String(Day(ld_Fecemb),'00')
			ls_Registro	+=	'-'
			ls_Registro	+=	String(Month(ld_Fecemb),'00')
			ls_Registro	+=	'-'
			ls_Registro	+=	String(Year(ld_Fecemb)) 
			ls_Registro	+=	String(ll_Productor, '00000')
			ls_Registro	+=	String(li_Especie, '00')
			ls_Registro	+=	String(li_Etiqueta, '0000')
			ls_Registro	+=	String(li_Variedad, '0000')
			IF Len(Trim(ls_Calibre)) < 3 THEN
				ls_Registro	+=	Trim(ls_Calibre) + Fill(' ' , 3 - Len(Trim(ls_Calibre)))
			ELSE
				ls_Registro += Mid(Trim(ls_Calibre),1,3)
			END IF
			ls_Registro	+=	String(ll_Cajas, '000')		

			ll_FilaDet	=	dw_1.InsertRow(0)			
			dw_1.Object.registro[ll_FilaDet]	=	ls_Registro			
		NEXT
	END IF
	
	//	Fin de Archivo
	
//	IF dw_1.SaveAs(gs_disco+":\GeneradosSAG\" + ls_Archivo, Text!, False) = -1 THEN
	IF dw_1.SaveAs(gs_disco+":\GeneradosSAAM\" + ls_Archivo, Text! , False) = -1 THEN
//	IF dw_1.SaveAs("C:\pASO\" + ls_Archivo, Excel!, False) = -1 THEN
		MessageBox("Atención","No se pudo generar el archivo " + ls_Archivo)
	ELSE
		sle_mensa.text	= "Archivo " + ls_Archivo + " Generado. Avise a Computación."
	END IF
	
END IF


end event

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

on w_gene_archivo_recepciones_cabrini.create
this.uo_selcliente=create uo_selcliente
this.dw_2=create dw_2
this.em_desde=create em_desde
this.st_2=create st_2
this.st_numero=create st_numero
this.st_6=create st_6
this.st_3=create st_3
this.sle_mensa=create sle_mensa
this.em_hasta=create em_hasta
this.dw_1=create dw_1
this.st_5=create st_5
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.st_1=create st_1
this.Control[]={this.uo_selcliente,&
this.dw_2,&
this.em_desde,&
this.st_2,&
this.st_numero,&
this.st_6,&
this.st_3,&
this.sle_mensa,&
this.em_hasta,&
this.dw_1,&
this.st_5,&
this.pb_salir,&
this.pb_grabar,&
this.st_1}
end on

on w_gene_archivo_recepciones_cabrini.destroy
destroy(this.uo_selcliente)
destroy(this.dw_2)
destroy(this.em_desde)
destroy(this.st_2)
destroy(this.st_numero)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.sle_mensa)
destroy(this.em_hasta)
destroy(this.dw_1)
destroy(this.st_5)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.st_1)
end on

event open;x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

uo_SelCliente.Seleccion(False, False)
uo_SelCliente.Inicia(gi_CodExport)

em_Desde.Text	=	String(Today())
em_Hasta.Text		=	String(Today())

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)

end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_recepciones_cabrini
integer x = 754
integer y = 320
integer height = 100
integer taborder = 20
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type dw_2 from datawindow within w_gene_archivo_recepciones_cabrini
boolean visible = false
integer x = 1211
integer y = 1116
integer width = 686
integer height = 400
integer taborder = 70
string title = "none"
string dataobject = "dw_genera_archpla_recepcion_cabrini"
boolean livescroll = true
end type

type em_desde from editmask within w_gene_archivo_recepciones_cabrini
integer x = 754
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

event modified;IF Date(em_Hasta.Text) <  Date(This.Text) THEN
	MessageBox("Atención", "La Fecha Debe ser Menor o Igual a la Fecha Hasta.~r~r" + &
				  "Ingrese Otra.", Exclamation!, Ok!)
	This.Text					=	String(Today())
	This.SetFocus()
END IF


end event

type st_2 from statictext within w_gene_archivo_recepciones_cabrini
integer x = 137
integer y = 744
integer width = 485
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

type st_numero from statictext within w_gene_archivo_recepciones_cabrini
integer x = 137
integer y = 596
integer width = 485
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

type st_6 from statictext within w_gene_archivo_recepciones_cabrini
integer x = 82
integer y = 920
integer width = 1970
integer height = 224
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within w_gene_archivo_recepciones_cabrini
integer x = 142
integer y = 336
integer width = 311
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

type sle_mensa from singlelineedit within w_gene_archivo_recepciones_cabrini
integer x = 133
integer y = 976
integer width = 1879
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
long textcolor = 65535
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type em_hasta from editmask within w_gene_archivo_recepciones_cabrini
integer x = 754
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

event modified;IF Date(em_Desde.Text) >  Date(This.Text) THEN
	MessageBox("Atención", "La Fecha Debe ser Mayor o Igual a la Fecha Desde.~r~r" + &
				  "Ingrese Otra.", Exclamation!, Ok!)
	This.Text					=	em_Desde.Text
END IF


end event

type dw_1 from datawindow within w_gene_archivo_recepciones_cabrini
boolean visible = false
integer x = 137
integer y = 1164
integer width = 1006
integer height = 400
string dataobject = "dw_genera_plano_recepcion_cabrini"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
end type

type st_5 from statictext within w_gene_archivo_recepciones_cabrini
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
string text = "Genera Plano Recepciones Cabrini"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_gene_archivo_recepciones_cabrini
integer x = 2162
integer y = 916
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

type pb_grabar from picturebutton within w_gene_archivo_recepciones_cabrini
integer x = 2162
integer y = 624
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

type st_1 from statictext within w_gene_archivo_recepciones_cabrini
integer x = 82
integer y = 224
integer width = 1970
integer height = 692
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

