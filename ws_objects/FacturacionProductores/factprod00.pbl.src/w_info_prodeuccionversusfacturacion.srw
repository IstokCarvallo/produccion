$PBExportHeader$w_info_prodeuccionversusfacturacion.srw
forward
global type w_info_prodeuccionversusfacturacion from w_para_informes
end type
type st_1 from statictext within w_info_prodeuccionversusfacturacion
end type
type st_6 from statictext within w_info_prodeuccionversusfacturacion
end type
type st_3 from statictext within w_info_prodeuccionversusfacturacion
end type
type st_8 from statictext within w_info_prodeuccionversusfacturacion
end type
type st_5 from statictext within w_info_prodeuccionversusfacturacion
end type
type st_14 from statictext within w_info_prodeuccionversusfacturacion
end type
type st_15 from statictext within w_info_prodeuccionversusfacturacion
end type
type st_fechaembarque from statictext within w_info_prodeuccionversusfacturacion
end type
type em_hasta from editmask within w_info_prodeuccionversusfacturacion
end type
type em_desde from editmask within w_info_prodeuccionversusfacturacion
end type
type st_2 from statictext within w_info_prodeuccionversusfacturacion
end type
type st_4 from statictext within w_info_prodeuccionversusfacturacion
end type
type st_variedad from statictext within w_info_prodeuccionversusfacturacion
end type
type cbx_consvariedad from checkbox within w_info_prodeuccionversusfacturacion
end type
type st_calidad from statictext within w_info_prodeuccionversusfacturacion
end type
type cbx_conscalidad from checkbox within w_info_prodeuccionversusfacturacion
end type
type st_7 from statictext within w_info_prodeuccionversusfacturacion
end type
type cbx_1 from checkbox within w_info_prodeuccionversusfacturacion
end type
type st_9 from statictext within w_info_prodeuccionversusfacturacion
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_prodeuccionversusfacturacion
end type
type uo_selplanta from uo_seleccion_plantas within w_info_prodeuccionversusfacturacion
end type
type uo_selespecie from uo_seleccion_especie within w_info_prodeuccionversusfacturacion
end type
type uo_selproductor from uo_seleccion_productor within w_info_prodeuccionversusfacturacion
end type
end forward

global type w_info_prodeuccionversusfacturacion from w_para_informes
integer x = 14
integer y = 32
integer width = 3378
integer height = 1532
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
st_5 st_5
st_14 st_14
st_15 st_15
st_fechaembarque st_fechaembarque
em_hasta em_hasta
em_desde em_desde
st_2 st_2
st_4 st_4
st_variedad st_variedad
cbx_consvariedad cbx_consvariedad
st_calidad st_calidad
cbx_conscalidad cbx_conscalidad
st_7 st_7
cbx_1 cbx_1
st_9 st_9
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
uo_selespecie uo_selespecie
uo_selproductor uo_selproductor
end type
global w_info_prodeuccionversusfacturacion w_info_prodeuccionversusfacturacion

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_especie, idwc_productor, idwc_packing, idwc_zona

String is_NomPlanta
Date		id_fechaini, id_fechafin
end variables

forward prototypes
public function boolean existepacking (integer li_cliente, ref string ls_columna)
public function boolean existeespecie (integer cliente, integer especie)
public function boolean existeproductor (integer li_cliente, long ll_productor)
public function boolean rangotemporada ()
end prototypes

public function boolean existepacking (integer li_cliente, ref string ls_columna);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dbo.plantadesp
WHERE	plde_codigo =  :ls_Columna;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Planta")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[7] = ls_columna	
	RETURN True 
END IF
end function

public function boolean existeespecie (integer cliente, integer especie);String		ls_Nombre

SELECT	espe_nombre
	INTO	:ls_Nombre
	FROM	dbo.especies
	WHERE	espe_codigo	=	:Especie ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Especies")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Especie no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	RETURN True
END IF
end function

public function boolean existeproductor (integer li_cliente, long ll_productor);String	ls_Nombre

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dbo.productores
	WHERE	prod_codigo	=	:ll_Productor ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Productor no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[6] = String(ll_Productor)	
	RETURN True
END IF
end function

public function boolean rangotemporada ();
SELECT	pate_inicio,pate_termin
INTO		:id_fechaini, :id_fechafin
FROM		dbo.paramtemporada
WHERE		pate_vigent = 1;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla paramtemporada")
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
end function

on w_info_prodeuccionversusfacturacion.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_6=create st_6
this.st_3=create st_3
this.st_8=create st_8
this.st_5=create st_5
this.st_14=create st_14
this.st_15=create st_15
this.st_fechaembarque=create st_fechaembarque
this.em_hasta=create em_hasta
this.em_desde=create em_desde
this.st_2=create st_2
this.st_4=create st_4
this.st_variedad=create st_variedad
this.cbx_consvariedad=create cbx_consvariedad
this.st_calidad=create st_calidad
this.cbx_conscalidad=create cbx_conscalidad
this.st_7=create st_7
this.cbx_1=create cbx_1
this.st_9=create st_9
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.uo_selespecie=create uo_selespecie
this.uo_selproductor=create uo_selproductor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.st_8
this.Control[iCurrent+5]=this.st_5
this.Control[iCurrent+6]=this.st_14
this.Control[iCurrent+7]=this.st_15
this.Control[iCurrent+8]=this.st_fechaembarque
this.Control[iCurrent+9]=this.em_hasta
this.Control[iCurrent+10]=this.em_desde
this.Control[iCurrent+11]=this.st_2
this.Control[iCurrent+12]=this.st_4
this.Control[iCurrent+13]=this.st_variedad
this.Control[iCurrent+14]=this.cbx_consvariedad
this.Control[iCurrent+15]=this.st_calidad
this.Control[iCurrent+16]=this.cbx_conscalidad
this.Control[iCurrent+17]=this.st_7
this.Control[iCurrent+18]=this.cbx_1
this.Control[iCurrent+19]=this.st_9
this.Control[iCurrent+20]=this.uo_selcliente
this.Control[iCurrent+21]=this.uo_selplanta
this.Control[iCurrent+22]=this.uo_selespecie
this.Control[iCurrent+23]=this.uo_selproductor
end on

on w_info_prodeuccionversusfacturacion.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.st_8)
destroy(this.st_5)
destroy(this.st_14)
destroy(this.st_15)
destroy(this.st_fechaembarque)
destroy(this.em_hasta)
destroy(this.em_desde)
destroy(this.st_2)
destroy(this.st_4)
destroy(this.st_variedad)
destroy(this.cbx_consvariedad)
destroy(this.st_calidad)
destroy(this.cbx_conscalidad)
destroy(this.st_7)
destroy(this.cbx_1)
destroy(this.st_9)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.uo_selespecie)
destroy(this.uo_selproductor)
end on

event open;call super::open;Date		ld_fechain, ld_fechate
Boolean	lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(True, True)
	uo_SelEspecie.Seleccion(True, True)
	uo_SelProductor.Seleccion(True, True)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Filtra(1)
	
	RangoTemporada()
	
	em_desde.Text				=	String(id_fechaini)
	em_hasta.Text				=	String(Today())
	
	istr_mant.argumento[21] =  '-9' //calibre
	istr_mant.argumento[22] =  '-9' //embalaje
End If
end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_prodeuccionversusfacturacion
integer x = 3022
integer y = 336
end type

type st_computador from w_para_informes`st_computador within w_info_prodeuccionversusfacturacion
end type

type st_usuario from w_para_informes`st_usuario within w_info_prodeuccionversusfacturacion
end type

type st_temporada from w_para_informes`st_temporada within w_info_prodeuccionversusfacturacion
end type

type p_logo from w_para_informes`p_logo within w_info_prodeuccionversusfacturacion
end type

type st_titulo from w_para_informes`st_titulo within w_info_prodeuccionversusfacturacion
integer x = 242
integer width = 2647
string text = "Comparativo Facturacion v/s Producción"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_prodeuccionversusfacturacion
integer x = 3008
integer y = 676
integer taborder = 240
string powertiptext = "Imprime Informe"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_valor, li_calibre
String		ls_dias, ls_fechas
Date		ld_hasta,ld_desde

istr_info.titulo	= 'HISTORICO DEL PRODUCTOR'
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_comparativo_prod_fact"

li_calibre    		=  Integer(istr_mant.argumento[21])
li_valor			=  Integer(istr_mant.argumento[22])

ld_hasta			=	Date(em_hasta.Text)
ld_desde			=	Date(em_desde.Text)

ls_fechas      =  "Desde el "+String(ld_desde)+" Hasta el "+String(ld_hasta)

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ld_desde, ld_hasta, uo_SelProductor.Codigo,& 
						uo_SelEspecie.Codigo, -1, li_calibre, li_valor)

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

type pb_salir from w_para_informes`pb_salir within w_info_prodeuccionversusfacturacion
integer x = 3008
integer y = 964
integer taborder = 250
string powertiptext = "Salir de la Ventana"
end type

type st_1 from statictext within w_info_prodeuccionversusfacturacion
integer x = 274
integer y = 716
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

type st_6 from statictext within w_info_prodeuccionversusfacturacion
integer x = 274
integer y = 508
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

type st_3 from statictext within w_info_prodeuccionversusfacturacion
integer x = 1673
integer y = 572
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

type st_8 from statictext within w_info_prodeuccionversusfacturacion
integer x = 274
integer y = 944
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

type st_5 from statictext within w_info_prodeuccionversusfacturacion
integer x = 1632
integer y = 436
integer width = 1257
integer height = 292
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

type st_14 from statictext within w_info_prodeuccionversusfacturacion
integer x = 242
integer y = 436
integer width = 1385
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

type st_15 from statictext within w_info_prodeuccionversusfacturacion
integer x = 242
integer y = 1076
integer width = 2647
integer height = 184
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

type st_fechaembarque from statictext within w_info_prodeuccionversusfacturacion
integer x = 1824
integer y = 1140
integer width = 224
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
string text = " Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_prodeuccionversusfacturacion
integer x = 2062
integer y = 1124
integer width = 393
integer height = 96
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
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
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

type em_desde from editmask within w_info_prodeuccionversusfacturacion
integer x = 736
integer y = 1124
integer width = 393
integer height = 96
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
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
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

type st_2 from statictext within w_info_prodeuccionversusfacturacion
integer x = 453
integer y = 1140
integer width = 256
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
string text = "Desde"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_prodeuccionversusfacturacion
integer x = 242
integer y = 836
integer width = 1385
integer height = 240
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

type st_variedad from statictext within w_info_prodeuccionversusfacturacion
integer x = 1673
integer y = 752
integer width = 302
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
string text = "Variedad"
boolean focusrectangle = false
end type

type cbx_consvariedad from checkbox within w_info_prodeuccionversusfacturacion
integer x = 2331
integer y = 748
integer width = 517
integer height = 80
integer taborder = 120
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
	istr_mant.argumento[20]	=	'-9'
ELSE
	istr_mant.argumento[20]	=	'-1'
END IF

end event

type st_calidad from statictext within w_info_prodeuccionversusfacturacion
integer x = 1669
integer y = 844
integer width = 256
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
boolean enabled = false
string text = "Calibre"
boolean focusrectangle = false
end type

type cbx_conscalidad from checkbox within w_info_prodeuccionversusfacturacion
integer x = 2331
integer y = 844
integer width = 517
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
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[21]	=	'-9'
ELSE
	istr_mant.argumento[21]	=	'-1'
END IF

end event

type st_7 from statictext within w_info_prodeuccionversusfacturacion
integer x = 1673
integer y = 936
integer width = 302
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
string text = "Valor"
boolean focusrectangle = false
end type

type cbx_1 from checkbox within w_info_prodeuccionversusfacturacion
integer x = 2331
integer y = 936
integer width = 517
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
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[22]	=	'-9'
ELSE
	istr_mant.argumento[22]	=	'-1'
END IF

end event

type st_9 from statictext within w_info_prodeuccionversusfacturacion
integer x = 1632
integer y = 728
integer width = 1257
integer height = 348
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

type uo_selcliente from uo_seleccion_clientesprod within w_info_prodeuccionversusfacturacion
integer x = 631
integer y = 492
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_prodeuccionversusfacturacion
integer x = 631
integer y = 624
integer taborder = 110
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_info_prodeuccionversusfacturacion
integer x = 1952
integer y = 480
integer taborder = 30
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_info_prodeuccionversusfacturacion
integer x = 631
integer y = 860
integer taborder = 170
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

