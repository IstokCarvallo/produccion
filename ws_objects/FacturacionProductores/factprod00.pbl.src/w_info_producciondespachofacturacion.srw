$PBExportHeader$w_info_producciondespachofacturacion.srw
forward
global type w_info_producciondespachofacturacion from w_para_informes
end type
type st_1 from statictext within w_info_producciondespachofacturacion
end type
type st_6 from statictext within w_info_producciondespachofacturacion
end type
type st_3 from statictext within w_info_producciondespachofacturacion
end type
type st_8 from statictext within w_info_producciondespachofacturacion
end type
type st_14 from statictext within w_info_producciondespachofacturacion
end type
type st_15 from statictext within w_info_producciondespachofacturacion
end type
type st_fechaembarque from statictext within w_info_producciondespachofacturacion
end type
type em_hasta from editmask within w_info_producciondespachofacturacion
end type
type em_desde from editmask within w_info_producciondespachofacturacion
end type
type st_2 from statictext within w_info_producciondespachofacturacion
end type
type st_4 from statictext within w_info_producciondespachofacturacion
end type
type st_9 from statictext within w_info_producciondespachofacturacion
end type
type st_variedad from statictext within w_info_producciondespachofacturacion
end type
type st_calidad from statictext within w_info_producciondespachofacturacion
end type
type cbx_calidad from checkbox within w_info_producciondespachofacturacion
end type
type em_calidad from editmask within w_info_producciondespachofacturacion
end type
type cbx_conscalidad from checkbox within w_info_producciondespachofacturacion
end type
type st_7 from statictext within w_info_producciondespachofacturacion
end type
type st_embalaje from statictext within w_info_producciondespachofacturacion
end type
type cbx_embalaje from checkbox within w_info_producciondespachofacturacion
end type
type cbx_consembalaje from checkbox within w_info_producciondespachofacturacion
end type
type cb_buscaembalaje from commandbutton within w_info_producciondespachofacturacion
end type
type em_embalaje from editmask within w_info_producciondespachofacturacion
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_producciondespachofacturacion
end type
type uo_selespecie from uo_seleccion_especie within w_info_producciondespachofacturacion
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_producciondespachofacturacion
end type
type uo_selplanta from uo_seleccion_plantas within w_info_producciondespachofacturacion
end type
type st_5 from statictext within w_info_producciondespachofacturacion
end type
type uo_selzonas from uo_seleccion_zonas within w_info_producciondespachofacturacion
end type
type uo_selproductor from uo_seleccion_productor within w_info_producciondespachofacturacion
end type
end forward

global type w_info_producciondespachofacturacion from w_para_informes
integer x = 14
integer y = 32
integer width = 3721
integer height = 1772
string title = "Comparativo Facturado v/s Producido"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_6 st_6
st_3 st_3
st_8 st_8
st_14 st_14
st_15 st_15
st_fechaembarque st_fechaembarque
em_hasta em_hasta
em_desde em_desde
st_2 st_2
st_4 st_4
st_9 st_9
st_variedad st_variedad
st_calidad st_calidad
cbx_calidad cbx_calidad
em_calidad em_calidad
cbx_conscalidad cbx_conscalidad
st_7 st_7
st_embalaje st_embalaje
cbx_embalaje cbx_embalaje
cbx_consembalaje cbx_consembalaje
cb_buscaembalaje cb_buscaembalaje
em_embalaje em_embalaje
uo_selcliente uo_selcliente
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
uo_selplanta uo_selplanta
st_5 st_5
uo_selzonas uo_selzonas
uo_selproductor uo_selproductor
end type
global w_info_producciondespachofacturacion w_info_producciondespachofacturacion

type variables
str_busqueda istr_busq
str_mant istr_mant

Date		id_fechaini, id_fechafin

uo_calibre	iuo_calibre
end variables

forward prototypes
public function boolean rangotemporada ()
end prototypes

public function boolean rangotemporada ();
SELECT	pate_inicio,pate_termin
INTO		:id_fechaini, :id_fechafin
FROM		dbo.paramtemporada
WHERE	pate_vigent = 1;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla paramtemporada")
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
end function

on w_info_producciondespachofacturacion.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_6=create st_6
this.st_3=create st_3
this.st_8=create st_8
this.st_14=create st_14
this.st_15=create st_15
this.st_fechaembarque=create st_fechaembarque
this.em_hasta=create em_hasta
this.em_desde=create em_desde
this.st_2=create st_2
this.st_4=create st_4
this.st_9=create st_9
this.st_variedad=create st_variedad
this.st_calidad=create st_calidad
this.cbx_calidad=create cbx_calidad
this.em_calidad=create em_calidad
this.cbx_conscalidad=create cbx_conscalidad
this.st_7=create st_7
this.st_embalaje=create st_embalaje
this.cbx_embalaje=create cbx_embalaje
this.cbx_consembalaje=create cbx_consembalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.em_embalaje=create em_embalaje
this.uo_selcliente=create uo_selcliente
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.uo_selplanta=create uo_selplanta
this.st_5=create st_5
this.uo_selzonas=create uo_selzonas
this.uo_selproductor=create uo_selproductor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.st_8
this.Control[iCurrent+5]=this.st_14
this.Control[iCurrent+6]=this.st_15
this.Control[iCurrent+7]=this.st_fechaembarque
this.Control[iCurrent+8]=this.em_hasta
this.Control[iCurrent+9]=this.em_desde
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.st_4
this.Control[iCurrent+12]=this.st_9
this.Control[iCurrent+13]=this.st_variedad
this.Control[iCurrent+14]=this.st_calidad
this.Control[iCurrent+15]=this.cbx_calidad
this.Control[iCurrent+16]=this.em_calidad
this.Control[iCurrent+17]=this.cbx_conscalidad
this.Control[iCurrent+18]=this.st_7
this.Control[iCurrent+19]=this.st_embalaje
this.Control[iCurrent+20]=this.cbx_embalaje
this.Control[iCurrent+21]=this.cbx_consembalaje
this.Control[iCurrent+22]=this.cb_buscaembalaje
this.Control[iCurrent+23]=this.em_embalaje
this.Control[iCurrent+24]=this.uo_selcliente
this.Control[iCurrent+25]=this.uo_selespecie
this.Control[iCurrent+26]=this.uo_selvariedad
this.Control[iCurrent+27]=this.uo_selplanta
this.Control[iCurrent+28]=this.st_5
this.Control[iCurrent+29]=this.uo_selzonas
this.Control[iCurrent+30]=this.uo_selproductor
end on

on w_info_producciondespachofacturacion.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.st_8)
destroy(this.st_14)
destroy(this.st_15)
destroy(this.st_fechaembarque)
destroy(this.em_hasta)
destroy(this.em_desde)
destroy(this.st_2)
destroy(this.st_4)
destroy(this.st_9)
destroy(this.st_variedad)
destroy(this.st_calidad)
destroy(this.cbx_calidad)
destroy(this.em_calidad)
destroy(this.cbx_conscalidad)
destroy(this.st_7)
destroy(this.st_embalaje)
destroy(this.cbx_embalaje)
destroy(this.cbx_consembalaje)
destroy(this.cb_buscaembalaje)
destroy(this.em_embalaje)
destroy(this.uo_selcliente)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.uo_selplanta)
destroy(this.st_5)
destroy(this.uo_selzonas)
destroy(this.uo_selproductor)
end on

event open;call super::open;Date		ld_fechain, ld_fechate
Boolean	lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelZonas.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelVariedad.Codigo) Then lb_Cerrar = True


If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(True, True)
	uo_SelZonas.Seleccion(True, True)
	uo_SelProductor.Seleccion(True, True)
	uo_SelEspecie.Seleccion(True, True)
	uo_SelVariedad.Seleccion(True, True)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Filtra(1)
	
	RangoTemporada()
	
	em_desde.Text				=	String(id_fechaini)
	em_hasta.Text				=	String(Today())
	
	iuo_calibre   				=	Create uo_calibre

	istr_mant.argumento[21] =  '-9' //calibre
	istr_mant.argumento[22] =  '-9' //embalaje
End If
end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_producciondespachofacturacion
integer x = 3273
integer y = 604
end type

type st_computador from w_para_informes`st_computador within w_info_producciondespachofacturacion
integer y = 140
end type

type st_usuario from w_para_informes`st_usuario within w_info_producciondespachofacturacion
end type

type st_temporada from w_para_informes`st_temporada within w_info_producciondespachofacturacion
end type

type p_logo from w_para_informes`p_logo within w_info_producciondespachofacturacion
end type

type st_titulo from w_para_informes`st_titulo within w_info_producciondespachofacturacion
integer width = 2971
string text = "Comparativo Facturado v/s Producido"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_producciondespachofacturacion
integer x = 3351
integer y = 1020
integer taborder = 240
string powertiptext = "Imprime Informe"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila
String		ls_dias, ls_fechas, ls_calibre, ls_embalaje
Date		ld_hasta,ld_desde

istr_info.titulo	= 'HISTORICO DEL PRODUCTOR'

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_producciondespachofacturacion"

ls_calibre     =  istr_mant.argumento[21]
ls_embalaje  =  istr_mant.argumento[22]

ld_hasta			=	Date(em_hasta.Text)
ld_desde			=	Date(em_desde.Text)

ls_fechas      =  "Desde el "+String(ld_desde)+" Hasta el "+String(ld_hasta)

vinf.dw_1.SetTransObject(sqlca)
fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelZonas.Codigo, uo_SelPlanta.Codigo, uo_SelEspecie.Codigo, &
						uo_SelProductor.Codigo, ld_desde, ld_hasta, uo_SelVariedad.Codigo,ls_calibre,ls_embalaje)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("Fechas.text = '" + ls_fechas + "'")
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_producciondespachofacturacion
integer x = 3351
integer y = 1308
integer taborder = 250
string powertiptext = "Salir de la Ventana"
end type

type st_1 from statictext within w_info_producciondespachofacturacion
integer x = 270
integer y = 700
integer width = 306
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
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_producciondespachofacturacion
integer x = 270
integer y = 516
integer width = 306
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

type st_3 from statictext within w_info_producciondespachofacturacion
integer x = 1847
integer y = 544
integer width = 279
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

type st_8 from statictext within w_info_producciondespachofacturacion
integer x = 270
integer y = 1196
integer width = 306
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
string text = "Productor"
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_producciondespachofacturacion
integer x = 238
integer y = 436
integer width = 1568
integer height = 400
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

type st_15 from statictext within w_info_producciondespachofacturacion
integer x = 238
integer y = 1324
integer width = 2976
integer height = 196
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

type st_fechaembarque from statictext within w_info_producciondespachofacturacion
integer x = 2112
integer y = 1396
integer width = 192
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Hasta"
boolean focusrectangle = false
boolean disabledlook = true
end type

type em_hasta from editmask within w_info_producciondespachofacturacion
integer x = 2313
integer y = 1384
integer width = 544
integer height = 88
integer taborder = 230
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;Date	ld_Fecter, ld_FecIni

IF This.Text <> '' THEN
	IF	date(This.Text) > id_fechafin THEN
			MessageBox("Error","Fecha NO puede ser Mayor a Fecha Fin Temporada",Information!, Ok!)
			This.Text = String(id_fechafin)
			RETURN
		END IF
		
		IF	date(This.Text) < id_fechaini THEN
			MessageBox("Error","Fecha NO puede ser Menor a Fecha Inicio Temporada",Information!, Ok!)
			This.Text = String(id_fechafin)
			RETURN
		END IF
		
		ld_FecIni	=	Date(em_desde.Text)
		ld_FecTer	=	date(This.Text)
		
		IF Not IsNull(ld_FecIni) THEN
			IF ld_FecIni > ld_Fecter THEN
				MessageBox("Error","Fecha NO puede ser Menor a Fecha Inicio.",Information!, Ok!)
				This.Text = String(id_fechafin)
				RETURN
			END IF
		END IF			
END IF	



end event

type em_desde from editmask within w_info_producciondespachofacturacion
integer x = 823
integer y = 1384
integer width = 521
integer height = 88
integer taborder = 220
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;Date	ld_Fecter, ld_FecIni

IF This.Text <> '' THEN
	IF	date(This.Text) < id_fechaini THEN
		MessageBox("Error","Fecha NO puede ser Menor a Fecha Inicio Temporada",Information!, Ok!)
		This.Text = string(id_fechaini)
		RETURN
	END IF	
	
	IF	date(This.Text) > id_fechafin THEN
		MessageBox("Error","Fecha NO puede ser Mayor a Fecha Término Temporada",Information!, Ok!)
		This.Text = string(id_fechaini)
		RETURN
	END IF			
			
	ld_Fecter	=	date(em_hasta.Text)
	ld_FecIni	=	date(This.Text)
	
	IF Not IsNull(ld_Fecter) THEN
		IF ld_FecIni > ld_Fecter THEN
			MessageBox("Error","Fecha NO puede ser Mayor a Fecha Término",Information!, Ok!)
			This.Text = string(id_fechaini)
			RETURN
		END IF
	END IF
END IF	




end event

type st_2 from statictext within w_info_producciondespachofacturacion
integer x = 562
integer y = 1392
integer width = 233
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Desde"
boolean focusrectangle = false
boolean disabledlook = true
end type

type st_4 from statictext within w_info_producciondespachofacturacion
integer x = 238
integer y = 836
integer width = 1568
integer height = 488
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

type st_9 from statictext within w_info_producciondespachofacturacion
integer x = 270
integer y = 948
integer width = 306
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
string text = "Zona"
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_producciondespachofacturacion
integer x = 1847
integer y = 748
integer width = 302
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type st_calidad from statictext within w_info_producciondespachofacturacion
integer x = 1847
integer y = 964
integer width = 256
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
string text = "Calidad"
boolean focusrectangle = false
end type

type cbx_calidad from checkbox within w_info_producciondespachofacturacion
integer x = 2171
integer y = 868
integer width = 297
integer height = 80
integer taborder = 150
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
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_conscalidad.Enabled	=	True
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[21]		=	'-1'
ELSE
	cbx_conscalidad.Enabled	=	False
	cbx_conscalidad.Checked	=	False
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

type em_calidad from editmask within w_info_producciondespachofacturacion
integer x = 2171
integer y = 952
integer width = 210
integer height = 96
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxx"
end type

event modified;Integer	li_cliente,	li_especie, li_variedad, li_cantid
String	ls_calibre

IF This.Text <> '' THEN
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.existe(uo_SelEspecie.Codigo, uo_SelVariedad.Codigo,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		istr_mant.argumento[21]	=	iuo_calibre.calibre
		em_calidad.Text				=	iuo_calibre.calibre
		Return 1
	END IF	
END IF	

end event

type cbx_conscalidad from checkbox within w_info_producciondespachofacturacion
integer x = 2633
integer y = 868
integer width = 471
integer height = 80
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[21]	=	'-9'
ELSE
	istr_mant.argumento[21]	=	'-1'
END IF

end event

type st_7 from statictext within w_info_producciondespachofacturacion
integer x = 1806
integer y = 1080
integer width = 1408
integer height = 244
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

type st_embalaje from statictext within w_info_producciondespachofacturacion
integer x = 1847
integer y = 1208
integer width = 288
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Embalaje"
boolean focusrectangle = false
end type

type cbx_embalaje from checkbox within w_info_producciondespachofacturacion
integer x = 2171
integer y = 1108
integer width = 402
integer height = 80
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_consembalaje.Enabled	=	True
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
	istr_mant.argumento[22]		=	'-1'
ELSE
	cbx_consembalaje.Enabled	=	False
	cbx_consembalaje.Checked	=	False
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
	em_embalaje.SetFocus()	
END IF
end event

type cbx_consembalaje from checkbox within w_info_producciondespachofacturacion
integer x = 2633
integer y = 1108
integer width = 471
integer height = 80
integer taborder = 190
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[22]	=	'-9'
ELSE
	istr_mant.argumento[22]	=	'-1'
END IF

end event

type cb_buscaembalaje from commandbutton within w_info_producciondespachofacturacion
integer x = 2395
integer y = 1204
integer width = 96
integer height = 84
integer taborder = 210
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "..."
end type

event clicked;
Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	String(uo_SelCliente.Codigo)

OpenWithParm(w_busc_embalajesprod, lstr_busq)
lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
	istr_mant.argumento[22]	=	lstr_busq.argum[2]
END IF
end event

type em_embalaje from editmask within w_info_producciondespachofacturacion
integer x = 2171
integer y = 1196
integer width = 210
integer height = 96
integer taborder = 200
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxxxxxxxxx"
end type

event modified;String		ls_embalaje, ls_Nombre
ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dbo.embalajesprod
	WHERE	clie_codigo	=	:uo_SelCliente.Codigo
	AND	emba_codigo	=	:ls_embalaje;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Embalajes")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Embalaje no ha sido Definido.~r~rIngrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	istr_mant.argumento[22]	=	ls_embalaje
END IF
end event

type uo_selcliente from uo_seleccion_clientesprod within w_info_producciondespachofacturacion
integer x = 626
integer y = 500
integer height = 100
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_info_producciondespachofacturacion
integer x = 2171
integer y = 448
integer taborder = 30
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case  -1, -9
		uo_SelVariedad.Filtra(-1)
		
	Case Else
		uo_SelVariedad.Filtra(This.Codigo)
		
End Choose
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_producciondespachofacturacion
integer x = 2171
integer y = 656
integer taborder = 20
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_producciondespachofacturacion
event destroy ( )
integer x = 626
integer y = 604
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_5 from statictext within w_info_producciondespachofacturacion
integer x = 1806
integer y = 436
integer width = 1408
integer height = 644
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

type uo_selzonas from uo_seleccion_zonas within w_info_producciondespachofacturacion
integer x = 626
integer y = 856
integer taborder = 160
boolean bringtotop = true
end type

on uo_selzonas.destroy
call uo_seleccion_zonas::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_info_producciondespachofacturacion
integer x = 626
integer y = 1104
integer taborder = 250
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

