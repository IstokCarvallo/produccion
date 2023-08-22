$PBExportHeader$w_proc_inspeccion_informada.srw
forward
global type w_proc_inspeccion_informada from window
end type
type st_9 from statictext within w_proc_inspeccion_informada
end type
type st_8 from statictext within w_proc_inspeccion_informada
end type
type st_5 from statictext within w_proc_inspeccion_informada
end type
type sle_detalle from singlelineedit within w_proc_inspeccion_informada
end type
type dw_descripcion from datawindow within w_proc_inspeccion_informada
end type
type dw_destino from datawindow within w_proc_inspeccion_informada
end type
type dw_especie from datawindow within w_proc_inspeccion_informada
end type
type em_fechainspec from editmask within w_proc_inspeccion_informada
end type
type st_3 from statictext within w_proc_inspeccion_informada
end type
type em_nroinspec from editmask within w_proc_inspeccion_informada
end type
type st_2 from statictext within w_proc_inspeccion_informada
end type
type st_1 from statictext within w_proc_inspeccion_informada
end type
type pb_salir from picturebutton within w_proc_inspeccion_informada
end type
end forward

global type w_proc_inspeccion_informada from window
integer width = 3470
integer height = 684
boolean titlebar = true
string title = "Datos de la Inspección"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 553648127
string icon = "AppIcon!"
event ue_guardar pbm_custom11
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
st_9 st_9
st_8 st_8
st_5 st_5
sle_detalle sle_detalle
dw_descripcion dw_descripcion
dw_destino dw_destino
dw_especie dw_especie
em_fechainspec em_fechainspec
st_3 st_3
em_nroinspec em_nroinspec
st_2 st_2
st_1 st_1
pb_salir pb_salir
end type
global w_proc_inspeccion_informada w_proc_inspeccion_informada

type variables
Boolean			ib_Anulacion
Integer			ii_PlantaSag, ii_CantTarjas, ii_CantInspec, il_fumiga
Date				id_FechaRepa
Date				id_FechaAcceso
Time				it_HoraAcceso
String			is_nombre, is_descripcion


DatawindowChild	dwc_especie, dwc_descripcion, dwc_mercado


end variables

forward prototypes
public function boolean existedestino (integer ai_destino)
public function boolean existedescripcion (integer ai_especie, integer ai_destino, integer ai_secuencia)
end prototypes

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function boolean existedestino (integer ai_destino);Integer	li_Cliente, li_Planta
Long		ll_Numero
String	ls

SELECT distinct dest_codigo, dest_fumbla, dest_nombre
INTO :ll_Numero, :il_fumiga, :is_nombre
FROM dbo.destinos
WHERE dest_codigo	=	:ai_destino;

IF IsNull(ll_Numero) OR ll_Numero = 0 THEN
	MessageBox("Cuidado","Destino No Existe")
	RETURN True
END IF
RETURN False	
end function

public function boolean existedescripcion (integer ai_especie, integer ai_destino, integer ai_secuencia);Integer	li_Cliente, li_Planta
Long		ll_Numero

SELECT dsag_descrip
INTO :is_descripcion
FROM dbo.destinossag
WHERE dest_codigo	=	:ai_destino
AND	espe_codigo =  :ai_especie
AND	dsag_secuen =  :ai_secuencia;

IF is_descripcion = '' THEN
	MessageBox("Cuidado","destinossag No Existe")
	RETURN False
END IF
RETURN True	
end function

on w_proc_inspeccion_informada.create
this.st_9=create st_9
this.st_8=create st_8
this.st_5=create st_5
this.sle_detalle=create sle_detalle
this.dw_descripcion=create dw_descripcion
this.dw_destino=create dw_destino
this.dw_especie=create dw_especie
this.em_fechainspec=create em_fechainspec
this.st_3=create st_3
this.em_nroinspec=create em_nroinspec
this.st_2=create st_2
this.st_1=create st_1
this.pb_salir=create pb_salir
this.Control[]={this.st_9,&
this.st_8,&
this.st_5,&
this.sle_detalle,&
this.dw_descripcion,&
this.dw_destino,&
this.dw_especie,&
this.em_fechainspec,&
this.st_3,&
this.em_nroinspec,&
this.st_2,&
this.st_1,&
this.pb_salir}
end on

on w_proc_inspeccion_informada.destroy
destroy(this.st_9)
destroy(this.st_8)
destroy(this.st_5)
destroy(this.sle_detalle)
destroy(this.dw_descripcion)
destroy(this.dw_destino)
destroy(this.dw_especie)
destroy(this.em_fechainspec)
destroy(this.st_3)
destroy(this.em_nroinspec)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.pb_salir)
end on

event open;x = 200//500
y = 300//1400

This.Icon	=	Gstr_apl.Icono

istr_mant3 = Message.PowerObjectParm

em_fechainspec.Text	   =	String(Today())
istr_mant3.argumento[2] =	String(Today())

dw_destino.GetChild("dest_codigo", dwc_mercado)
dwc_mercado.SetTransObject(Sqlca)
dwc_mercado.Retrieve(-1)
dw_destino.SetTransObject(Sqlca)
dw_destino.InsertRow(0)

dw_especie.GetChild("espe_codigo", dwc_especie)
dwc_especie.SetTransObject(Sqlca)
dwc_especie.Retrieve()			//especie
dw_especie.InsertRow(0)
dw_especie.Object.espe_codigo[1]	=	Integer(istr_mant3.argumento[3])

dw_descripcion.GetChild("dsag_secuen", dwc_descripcion)
dwc_descripcion.SetTransObject(Sqlca)
dwc_descripcion.Retrieve(Integer(istr_mant3.argumento[3]),-1)			//especie
dw_descripcion.InsertRow(1)



end event

type st_9 from statictext within w_proc_inspeccion_informada
integer x = 73
integer y = 232
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type st_8 from statictext within w_proc_inspeccion_informada
integer x = 73
integer y = 416
integer width = 453
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Detalle Destino"
boolean focusrectangle = false
end type

type st_5 from statictext within w_proc_inspeccion_informada
integer x = 1477
integer y = 232
integer width = 265
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Destino"
boolean focusrectangle = false
end type

type sle_detalle from singlelineedit within w_proc_inspeccion_informada
integer x = 526
integer y = 412
integer width = 2441
integer height = 92
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
integer limit = 600
borderstyle borderstyle = stylelowered!
end type

event modified;//istr_mant.argumento[7]	=	This.Text
end event

type dw_descripcion from datawindow within w_proc_inspeccion_informada
integer x = 2021
integer y = 312
integer width = 855
integer height = 92
integer taborder = 50
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_destinosag"
boolean border = false
boolean livescroll = true
end type

event itemchanged;IF existedescripcion(dw_especie.Object.espe_codigo[1],dw_destino.Object.dest_codigo[1],Integer(data)) THEN
	sle_detalle.Text = is_descripcion
	istr_mant3.argumento[6] = is_descripcion
	istr_mant3.argumento[7] = data
END IF
end event

type dw_destino from datawindow within w_proc_inspeccion_informada
integer x = 2021
integer y = 212
integer width = 882
integer height = 92
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_destinos"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_columna
Integer	li_null

SetNull(li_null)

ls_columna	= dwo.Name

CHOOSE CASE ls_columna
	CASE "dest_codigo"
		IF existedestino(Integer(data)) THEN
			dw_descripcion.SetItem(1, "dsag_secuen", li_null)
			This.SetItem(1, "dest_codigo", li_null)
			dw_descripcion.Enabled = False
			RETURN 1
		ELSE
			istr_mant3.argumento[5]	=	data
						
			dw_descripcion.GetChild("dsag_secuen", dwc_descripcion)
			dwc_descripcion.SetTransObject(Sqlca)
			
			IF isnull(dw_especie.Object.espe_codigo[1]) THEN
				dwc_descripcion.Retrieve(-1,integer(data))
				dw_descripcion.SetItem(1, "dsag_secuen", li_null)
			ELSE
				dwc_descripcion.Retrieve(dw_especie.Object.espe_codigo[1],integer(data))
				dw_descripcion.Enabled = True
			END IF	
		END IF	
		
END CHOOSE


end event

type dw_especie from datawindow within w_proc_inspeccion_informada
integer x = 526
integer y = 212
integer width = 878
integer height = 92
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;//String	ls_Columna[]
//Integer li_Nula
//
//IsNull(li_Nula)
//IF ExisteEspecie(ii_Cliente, Integer(data), ls_Columna[]) THEN
//	ii_Especie		=	Integer(data)
//	is_NomEspecie	=	ls_Columna[1]
//	
//	dw_descripcion.GetChild("dsag_secuen", dwc_descripcion)
//	dwc_descripcion.SetTransObject(Sqlca)
//	
//	IF isnull(dw_destino.Object.dest_codigo[1]) THEN
//		dwc_descripcion.Retrieve(integer(data),-1)
//		dw_descripcion.SetItem(1, "dsag_secuen", li_Nula)
//		dw_descripcion.Enabled = False
//	ELSE
//		dwc_descripcion.Retrieve(integer(data),dw_destino.Object.dest_codigo[1])
//		dw_descripcion.Enabled = True
//	END IF	
////	dw_descripcion.InsertRow(1)
//  
//ELSE
//	dw_descripcion.SetItem(1, "dsag_secuen", li_Nula)
//	dw_descripcion.Enabled = False
//	This.SetItem(1, "espe_codigo", li_Nula)
//	RETURN 1
//END IF
end event

type em_fechainspec from editmask within w_proc_inspeccion_informada
integer x = 2021
integer y = 104
integer width = 393
integer height = 100
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant3.argumento[2]=This.Text


end event

type st_3 from statictext within w_proc_inspeccion_informada
integer x = 1477
integer y = 104
integer width = 521
integer height = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Fecha Inspección"
boolean focusrectangle = false
end type

type em_nroinspec from editmask within w_proc_inspeccion_informada
integer x = 526
integer y = 104
integer width = 402
integer height = 100
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant3.argumento[1]	=	This.Text
end event

type st_2 from statictext within w_proc_inspeccion_informada
integer x = 73
integer y = 104
integer width = 402
integer height = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Inspección"
boolean focusrectangle = false
end type

type st_1 from statictext within w_proc_inspeccion_informada
integer x = 32
integer y = 40
integer width = 2971
integer height = 512
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

type pb_salir from picturebutton within w_proc_inspeccion_informada
integer x = 3109
integer y = 300
integer width = 302
integer height = 252
integer taborder = 40
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Cancelar.png"
alignment htextalign = left!
end type

event clicked;CloseWithReturn(Parent, istr_mant3)

end event

