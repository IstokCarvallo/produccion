$PBExportHeader$w_info_produccion_diaria.srw
$PBExportComments$Informe de Produccion diaria
forward
global type w_info_produccion_diaria from w_para_informes
end type
type st_4 from statictext within w_info_produccion_diaria
end type
type st_1 from statictext within w_info_produccion_diaria
end type
type st_2 from statictext within w_info_produccion_diaria
end type
type em_fech_ini from editmask within w_info_produccion_diaria
end type
type st_6 from statictext within w_info_produccion_diaria
end type
type st_3 from statictext within w_info_produccion_diaria
end type
type st_7 from statictext within w_info_produccion_diaria
end type
type em_nroguia from editmask within w_info_produccion_diaria
end type
type cbx_guia from checkbox within w_info_produccion_diaria
end type
type st_5 from statictext within w_info_produccion_diaria
end type
type st_productor from statictext within w_info_produccion_diaria
end type
type st_9 from statictext within w_info_produccion_diaria
end type
type cbx_consfru from checkbox within w_info_produccion_diaria
end type
type st_8 from statictext within w_info_produccion_diaria
end type
type dw_frurecep from datawindow within w_info_produccion_diaria
end type
type cbx_todosfru from checkbox within w_info_produccion_diaria
end type
type cbx_consolida from checkbox within w_info_produccion_diaria
end type
type st_10 from statictext within w_info_produccion_diaria
end type
type em_hasta from editmask within w_info_produccion_diaria
end type
type pb_excel1 from picturebutton within w_info_produccion_diaria
end type
type st_mensaje from statictext within w_info_produccion_diaria
end type
type uo_selespecie from uo_seleccion_especie within w_info_produccion_diaria
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_produccion_diaria
end type
type uo_selplantas from uo_seleccion_plantas within w_info_produccion_diaria
end type
type uo_selpacking from uo_seleccion_plantas within w_info_produccion_diaria
end type
type uo_selproductor from uo_seleccion_productor_cliente within w_info_produccion_diaria
end type
end forward

global type w_info_produccion_diaria from w_para_informes
integer x = 14
integer y = 32
integer width = 2830
integer height = 2252
string title = "Producción en Frigorificos"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_2 st_2
em_fech_ini em_fech_ini
st_6 st_6
st_3 st_3
st_7 st_7
em_nroguia em_nroguia
cbx_guia cbx_guia
st_5 st_5
st_productor st_productor
st_9 st_9
cbx_consfru cbx_consfru
st_8 st_8
dw_frurecep dw_frurecep
cbx_todosfru cbx_todosfru
cbx_consolida cbx_consolida
st_10 st_10
em_hasta em_hasta
pb_excel1 pb_excel1
st_mensaje st_mensaje
uo_selespecie uo_selespecie
uo_selcliente uo_selcliente
uo_selplantas uo_selplantas
uo_selpacking uo_selpacking
uo_selproductor uo_selproductor
end type
global w_info_produccion_diaria w_info_produccion_diaria

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_productor, idwc_pesoneto, idwc_fruta

uo_frutarecepcion   iuo_frutarecepcion
end variables

forward prototypes
public function string buscdescfruta (integer codigo)
end prototypes

public function string buscdescfruta (integer codigo);String	ls_descri


    SELECT frre_descri  
    INTO :ls_descri  
    FROM dbo.recfruprocee as re, dbo.frutarecibida as fr 
   WHERE re.frre_codigo = fr.frre_codigo and  
         re.frre_codigo = :codigo ;

		
RETURN ls_descri

end function

on w_info_produccion_diaria.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_2=create st_2
this.em_fech_ini=create em_fech_ini
this.st_6=create st_6
this.st_3=create st_3
this.st_7=create st_7
this.em_nroguia=create em_nroguia
this.cbx_guia=create cbx_guia
this.st_5=create st_5
this.st_productor=create st_productor
this.st_9=create st_9
this.cbx_consfru=create cbx_consfru
this.st_8=create st_8
this.dw_frurecep=create dw_frurecep
this.cbx_todosfru=create cbx_todosfru
this.cbx_consolida=create cbx_consolida
this.st_10=create st_10
this.em_hasta=create em_hasta
this.pb_excel1=create pb_excel1
this.st_mensaje=create st_mensaje
this.uo_selespecie=create uo_selespecie
this.uo_selcliente=create uo_selcliente
this.uo_selplantas=create uo_selplantas
this.uo_selpacking=create uo_selpacking
this.uo_selproductor=create uo_selproductor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_fech_ini
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.st_7
this.Control[iCurrent+8]=this.em_nroguia
this.Control[iCurrent+9]=this.cbx_guia
this.Control[iCurrent+10]=this.st_5
this.Control[iCurrent+11]=this.st_productor
this.Control[iCurrent+12]=this.st_9
this.Control[iCurrent+13]=this.cbx_consfru
this.Control[iCurrent+14]=this.st_8
this.Control[iCurrent+15]=this.dw_frurecep
this.Control[iCurrent+16]=this.cbx_todosfru
this.Control[iCurrent+17]=this.cbx_consolida
this.Control[iCurrent+18]=this.st_10
this.Control[iCurrent+19]=this.em_hasta
this.Control[iCurrent+20]=this.pb_excel1
this.Control[iCurrent+21]=this.st_mensaje
this.Control[iCurrent+22]=this.uo_selespecie
this.Control[iCurrent+23]=this.uo_selcliente
this.Control[iCurrent+24]=this.uo_selplantas
this.Control[iCurrent+25]=this.uo_selpacking
this.Control[iCurrent+26]=this.uo_selproductor
end on

on w_info_produccion_diaria.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_fech_ini)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.st_7)
destroy(this.em_nroguia)
destroy(this.cbx_guia)
destroy(this.st_5)
destroy(this.st_productor)
destroy(this.st_9)
destroy(this.cbx_consfru)
destroy(this.st_8)
destroy(this.dw_frurecep)
destroy(this.cbx_todosfru)
destroy(this.cbx_consolida)
destroy(this.st_10)
destroy(this.em_hasta)
destroy(this.pb_excel1)
destroy(this.st_mensaje)
destroy(this.uo_selespecie)
destroy(this.uo_selcliente)
destroy(this.uo_selplantas)
destroy(this.uo_selpacking)
destroy(this.uo_selproductor)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPacking.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	
	iuo_frutarecepcion	=	 CREATE uo_frutarecepcion//Esta en la libreria TabProdTab10   
	
	uo_SelCliente.Seleccion(False, False)
	uo_SelEspecie.Seleccion(True, True)
	uo_SelPlantas.Seleccion(True, False)
	uo_SelPacking.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	
	uo_SelCliente.Inicia(gi_CodExport)
//	uo_SelEspecie.Inicia(gi_CodEspecie)
	
	uo_SelPlantas.Filtra(1)
	uo_SelPacking.Filtra(2)
	uo_SelProductor.Filtra(gi_CodExport)
	
	dw_frurecep.GetChild("frre_codigo", idwc_fruta)
	idwc_fruta.SetTransObject(SQLCA)
	idwc_fruta.Retrieve()
	dw_frurecep.InsertRow(0)
	
	dw_frurecep.Enabled		=	False
	
	em_fech_ini.Text			=	String(Today())
	em_hasta.Text				=	String(Today())
	istr_mant.argumento[3]	= 	em_fech_ini.Text
	istr_mant.argumento[13]	= 	em_fech_ini.Text
	istr_mant.argumento[5]	=	"0"
	istr_mant.argumento[8]	=	"0"
End if
end event

type pb_excel from w_para_informes`pb_excel within w_info_produccion_diaria
end type

type st_computador from w_para_informes`st_computador within w_info_produccion_diaria
end type

type st_usuario from w_para_informes`st_usuario within w_info_produccion_diaria
end type

type st_temporada from w_para_informes`st_temporada within w_info_produccion_diaria
end type

type p_logo from w_para_informes`p_logo within w_info_produccion_diaria
end type

type st_titulo from w_para_informes`st_titulo within w_info_produccion_diaria
integer width = 2043
string text = "Informe Diario de Producción"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_produccion_diaria
string tag = "Imprimir Reporte"
integer x = 2446
integer y = 892
integer taborder = 100
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_fruta
Long		ll_nroguia
Date		ld_fecha_dia, ld_fecha_hasta
String		texto_fecha, ls_nroguia, ls_cajas,  ls_descri

istr_info.titulo	= 'INFORME DE PRODUCCION DIARIA'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_produccion_diaria"
vinf.dw_1.SetTransObject(sqlca)

ls_cajas	=	" Bultos"

IF cbx_consolida.Checked THEN
	ll_nroguia 	= -9
	ls_nroguia 	= 'CONSOLIDADA'
ELSEIF cbx_guia.checked  THEN
	ls_nroguia	=	'TODAS'
	ll_nroguia  =   -1
ELSE
	ll_nroguia	=	Long(em_nroguia.text)
	IF IsNull(ll_nroguia) OR ll_nroguia = 0 THEN
	   MessageBox("Atención","Debe Seleccionar una Guía Previamente",Exclamation!)
		em_nroguia.setfocus()
		RETURN
	ELSE
		ls_nroguia	=	String(ll_nroguia)
	END IF 
END IF

//Caracteristica de Recepción
IF cbx_consfru.Checked THEN
	li_fruta 	= -9
	ls_descri	= 'CONSOLIDADA'
ELSEIF cbx_todosfru.checked  THEN
	li_fruta  =   -1
	ls_descri	=	'TODAS'
ELSE
	li_fruta	=	dw_frurecep.Object.frre_codigo[1]
	IF IsNull(li_fruta) OR li_fruta = 0 THEN
	   MessageBox("Atención","Debe Seleccionar una Carácteristica Previamente",Exclamation!)
		dw_frurecep.setfocus()
		RETURN
	ELSE
		ls_descri	=	buscdescfruta(li_fruta)
	END IF 
END IF

ld_fecha_dia 	= 	Date(istr_mant.argumento[3])
ld_fecha_hasta	= 	Date(istr_mant.argumento[13])
texto_fecha		=  f_fecha_texto(String(ld_fecha_dia), 1)

fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlantas.Codigo, ld_fecha_dia, uo_SelEspecie.Codigo, ll_nroguia,&
					uo_SelProductor.Codigo,uo_SelPacking.Codigo,1,0,li_fruta,ld_fecha_hasta)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Object.titulo_informe.text = 'Producción Diaria'
	vinf.dw_1.Modify("fechas.text = '" + String(ld_fecha_dia) + " hasta " + String(ld_fecha_hasta) + "'")
	vinf.dw_1.Modify("guia.text = '" + ls_nroguia + "'")
	vinf.dw_1.Modify("Cajas.text = '" + ls_cajas + "'")
	vinf.dw_1.Modify("packing.text = '" + String(uo_SelPacking.Codigo, "00000") + "'")	
	vinf.dw_1.Modify("fruta.text = '" + ls_descri + "'")	
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_produccion_diaria
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2446
integer y = 1700
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_produccion_diaria
integer x = 251
integer y = 440
integer width = 2043
integer height = 820
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_produccion_diaria
integer x = 334
integer y = 728
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_produccion_diaria
integer x = 334
integer y = 1656
integer width = 553
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Recepción Desde  "
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fech_ini from editmask within w_info_produccion_diaria
integer x = 869
integer y = 1624
integer width = 421
integer height = 96
integer taborder = 70
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

event modified;istr_mant.argumento[3]	=	This.Text
end event

type st_6 from statictext within w_info_produccion_diaria
integer x = 334
integer y = 540
integer width = 233
integer height = 64
boolean bringtotop = true
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

type st_3 from statictext within w_info_produccion_diaria
integer x = 334
integer y = 1824
integer width = 462
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_produccion_diaria
integer x = 334
integer y = 1364
integer width = 475
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Recepción"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_nroguia from editmask within w_info_produccion_diaria
integer x = 864
integer y = 1332
integer width = 402
integer height = 96
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type cbx_guia from checkbox within w_info_produccion_diaria
integer x = 1449
integer y = 1340
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
end type

event clicked;call super::clicked;IF This.Checked THEN
   cbx_consolida.Checked   =  false
	em_nroguia.Enabled		=	False
	em_fech_ini.enabled=True
//	em_fech_ini.text = String(Today())
//	istr_mant.argumento[3] = String(Today())

ELSE
	em_nroguia.Enabled		=	True
	em_nroguia.SetFocus()
//	em_fech_ini.enabled=False	
//	em_fech_ini.text=String(Today())

END IF
end event

type st_5 from statictext within w_info_produccion_diaria
integer x = 251
integer y = 1260
integer width = 2043
integer height = 672
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_productor from statictext within w_info_produccion_diaria
integer x = 334
integer y = 924
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_produccion_diaria
integer x = 334
integer y = 1128
integer width = 238
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Packing"
alignment alignment = right!
boolean focusrectangle = false
end type

type cbx_consfru from checkbox within w_info_produccion_diaria
integer x = 1806
integer y = 1476
integer width = 443
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidada"
boolean checked = true
end type

event clicked;Integer li_Null
SetNull(li_null)

IF This.Checked THEN
	cbx_todosfru.Checked		=	False       
	dw_frurecep.Enabled		=	False
	dw_frurecep.Object.frre_codigo[1] = li_null
ELSE
	cbx_todosfru.Checked		=	true      
	dw_frurecep.Enabled		=	false
END IF
end event

type st_8 from statictext within w_info_produccion_diaria
integer x = 334
integer y = 1508
integer width = 521
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Caráct. de Recep."
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_frurecep from datawindow within w_info_produccion_diaria
integer x = 864
integer y = 1472
integer width = 567
integer height = 104
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_frutarecep"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_nula

IF NOT iuo_frutarecepcion.existe(Integer(data),True,sqlca) THEN
	This.SetItem(1, "frre_codigo", li_nula)
	RETURN 1
ELSE
	istr_mant.argumento[9] = data
END IF
end event

type cbx_todosfru from checkbox within w_info_produccion_diaria
integer x = 1449
integer y = 1476
integer width = 306
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
end type

event clicked;Integer li_null
SetNull(li_null)

call super::clicked;IF This.Checked THEN
   cbx_consfru.Checked   =  false
	dw_frurecep.Enabled		=	False
	dw_frurecep.Object.frre_codigo[1] = li_null
ELSE
	dw_frurecep.Enabled		=	True
	dw_frurecep.SetFocus()
END IF
end event

type cbx_consolida from checkbox within w_info_produccion_diaria
integer x = 1806
integer y = 1340
integer width = 443
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidada"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_guia.Checked			=	False       
	em_nroguia.Enabled		=	False
	em_fech_ini.enabled=True
//	em_fech_ini.text = String(Today())
//	istr_mant.argumento[3] = String(Today())
ELSE
	cbx_guia.Checked			=	true      
	em_nroguia.Enabled		=	false
END IF
end event

type st_10 from statictext within w_info_produccion_diaria
integer x = 1335
integer y = 1656
integer width = 215
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_produccion_diaria
integer x = 1568
integer y = 1624
integer width = 421
integer height = 96
integer taborder = 80
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

event modified;istr_mant.argumento[13]	=	This.Text
end event

type pb_excel1 from picturebutton within w_info_produccion_diaria
end type

type st_mensaje from statictext within w_info_produccion_diaria
integer x = 251
integer y = 1944
integer width = 2043
integer height = 112
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
boolean italic = true
long textcolor = 65535
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_produccion_diaria
event destroy ( )
integer x = 869
integer y = 1728
integer height = 176
integer taborder = 100
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_info_produccion_diaria
event destroy ( )
integer x = 869
integer y = 524
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplantas from uo_seleccion_plantas within w_info_produccion_diaria
integer x = 869
integer y = 620
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selpacking from uo_seleccion_plantas within w_info_produccion_diaria
event destroy ( )
integer x = 869
integer y = 1036
integer taborder = 50
boolean bringtotop = true
end type

on uo_selpacking.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selproductor from uo_seleccion_productor_cliente within w_info_produccion_diaria
integer x = 869
integer y = 824
integer taborder = 20
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_cliente::destroy
end on

