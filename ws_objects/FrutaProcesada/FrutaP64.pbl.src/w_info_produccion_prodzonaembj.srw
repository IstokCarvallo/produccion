$PBExportHeader$w_info_produccion_prodzonaembj.srw
forward
global type w_info_produccion_prodzonaembj from w_para_informes
end type
type st_4 from statictext within w_info_produccion_prodzonaembj
end type
type dw_cliente from datawindow within w_info_produccion_prodzonaembj
end type
type st_6 from statictext within w_info_produccion_prodzonaembj
end type
type st_8 from statictext within w_info_produccion_prodzonaembj
end type
type gb_3 from groupbox within w_info_produccion_prodzonaembj
end type
type st_5 from statictext within w_info_produccion_prodzonaembj
end type
type cbx_peso from checkbox within w_info_produccion_prodzonaembj
end type
type dw_pesoneto from datawindow within w_info_produccion_prodzonaembj
end type
type tit_peso from statictext within w_info_produccion_prodzonaembj
end type
type st_7 from statictext within w_info_produccion_prodzonaembj
end type
type st_9 from statictext within w_info_produccion_prodzonaembj
end type
type cbx_zonas from checkbox within w_info_produccion_prodzonaembj
end type
type dw_zonas from datawindow within w_info_produccion_prodzonaembj
end type
type st_1 from statictext within w_info_produccion_prodzonaembj
end type
type cbx_tipoproductor from checkbox within w_info_produccion_prodzonaembj
end type
type dw_tipoprodu from datawindow within w_info_produccion_prodzonaembj
end type
type st_2 from statictext within w_info_produccion_prodzonaembj
end type
type em_fech_ini from editmask within w_info_produccion_prodzonaembj
end type
type st_3 from statictext within w_info_produccion_prodzonaembj
end type
type em_fech_fin from editmask within w_info_produccion_prodzonaembj
end type
type st_10 from statictext within w_info_produccion_prodzonaembj
end type
type st_variedad from statictext within w_info_produccion_prodzonaembj
end type
type st_calidad from statictext within w_info_produccion_prodzonaembj
end type
type em_calidad from editmask within w_info_produccion_prodzonaembj
end type
type cbx_calidad from checkbox within w_info_produccion_prodzonaembj
end type
type cbx_conscalidad from checkbox within w_info_produccion_prodzonaembj
end type
type uo_selespecie from uo_seleccion_especie within w_info_produccion_prodzonaembj
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_produccion_prodzonaembj
end type
type cbx_varirotula from checkbox within w_info_produccion_prodzonaembj
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_produccion_prodzonaembj
end type
end forward

global type w_info_produccion_prodzonaembj from w_para_informes
integer x = 14
integer y = 32
integer width = 2702
integer height = 2440
string title = "Producción Productor/Variedad/Zonas/Semanas"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
event ue_imprimeresu ( )
st_4 st_4
dw_cliente dw_cliente
st_6 st_6
st_8 st_8
gb_3 gb_3
st_5 st_5
cbx_peso cbx_peso
dw_pesoneto dw_pesoneto
tit_peso tit_peso
st_7 st_7
st_9 st_9
cbx_zonas cbx_zonas
dw_zonas dw_zonas
st_1 st_1
cbx_tipoproductor cbx_tipoproductor
dw_tipoprodu dw_tipoprodu
st_2 st_2
em_fech_ini em_fech_ini
st_3 st_3
em_fech_fin em_fech_fin
st_10 st_10
st_variedad st_variedad
st_calidad st_calidad
em_calidad em_calidad
cbx_calidad cbx_calidad
cbx_conscalidad cbx_conscalidad
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
uo_selproductor uo_selproductor
end type
global w_info_produccion_prodzonaembj w_info_produccion_prodzonaembj

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta,idwc_zonas, &
						idwc_productor, idwc_packing, idwc_pesoneto, idwc_recibidor
						
String is_NomPlanta

uo_productores   						iuo_productores
uo_tipoproductor  					iuo_tipoproductor
uo_zonas  								iuo_zonas
uo_seleccion_especie					iuo_selespecie
uo_seleccion_variedad				iuo_selvariedad
uo_seleccion_varios_productores	iuo_selproductor
uo_calibre								iuo_calibre
end variables

forward prototypes
public function boolean existeproductor (integer li_cliente, long ll_productor)
end prototypes

public function boolean existeproductor (integer li_cliente, long ll_productor);String	ls_Nombre

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dba.productores
	WHERE	prod_codigo	=	:ll_Productor ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Productor no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[4] = String(ll_Productor)	
	RETURN True
END IF
end function

on w_info_produccion_prodzonaembj.create
int iCurrent
call super::create
this.st_4=create st_4
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.st_8=create st_8
this.gb_3=create gb_3
this.st_5=create st_5
this.cbx_peso=create cbx_peso
this.dw_pesoneto=create dw_pesoneto
this.tit_peso=create tit_peso
this.st_7=create st_7
this.st_9=create st_9
this.cbx_zonas=create cbx_zonas
this.dw_zonas=create dw_zonas
this.st_1=create st_1
this.cbx_tipoproductor=create cbx_tipoproductor
this.dw_tipoprodu=create dw_tipoprodu
this.st_2=create st_2
this.em_fech_ini=create em_fech_ini
this.st_3=create st_3
this.em_fech_fin=create em_fech_fin
this.st_10=create st_10
this.st_variedad=create st_variedad
this.st_calidad=create st_calidad
this.em_calidad=create em_calidad
this.cbx_calidad=create cbx_calidad
this.cbx_conscalidad=create cbx_conscalidad
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.uo_selproductor=create uo_selproductor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.st_8
this.Control[iCurrent+5]=this.gb_3
this.Control[iCurrent+6]=this.st_5
this.Control[iCurrent+7]=this.cbx_peso
this.Control[iCurrent+8]=this.dw_pesoneto
this.Control[iCurrent+9]=this.tit_peso
this.Control[iCurrent+10]=this.st_7
this.Control[iCurrent+11]=this.st_9
this.Control[iCurrent+12]=this.cbx_zonas
this.Control[iCurrent+13]=this.dw_zonas
this.Control[iCurrent+14]=this.st_1
this.Control[iCurrent+15]=this.cbx_tipoproductor
this.Control[iCurrent+16]=this.dw_tipoprodu
this.Control[iCurrent+17]=this.st_2
this.Control[iCurrent+18]=this.em_fech_ini
this.Control[iCurrent+19]=this.st_3
this.Control[iCurrent+20]=this.em_fech_fin
this.Control[iCurrent+21]=this.st_10
this.Control[iCurrent+22]=this.st_variedad
this.Control[iCurrent+23]=this.st_calidad
this.Control[iCurrent+24]=this.em_calidad
this.Control[iCurrent+25]=this.cbx_calidad
this.Control[iCurrent+26]=this.cbx_conscalidad
this.Control[iCurrent+27]=this.uo_selespecie
this.Control[iCurrent+28]=this.uo_selvariedad
this.Control[iCurrent+29]=this.cbx_varirotula
this.Control[iCurrent+30]=this.uo_selproductor
end on

on w_info_produccion_prodzonaembj.destroy
call super::destroy
destroy(this.st_4)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.st_8)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.cbx_peso)
destroy(this.dw_pesoneto)
destroy(this.tit_peso)
destroy(this.st_7)
destroy(this.st_9)
destroy(this.cbx_zonas)
destroy(this.dw_zonas)
destroy(this.st_1)
destroy(this.cbx_tipoproductor)
destroy(this.dw_tipoprodu)
destroy(this.st_2)
destroy(this.em_fech_ini)
destroy(this.st_3)
destroy(this.em_fech_fin)
destroy(this.st_10)
destroy(this.st_variedad)
destroy(this.st_calidad)
destroy(this.em_calidad)
destroy(this.cbx_calidad)
destroy(this.cbx_conscalidad)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.uo_selproductor)
end on

event open;call super::open;Boolean lb_Cerrar

x	=	0
y	=	0

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve(gi_CodExport)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

iuo_calibre   						=	Create uo_calibre

dw_zonas.GetChild("zona_codigo", idwc_zonas)
idwc_zonas.SetTransObject(sqlca)
idwc_zonas.Retrieve()
dw_zonas.InsertRow(0)

// uo_seleccion_productor
IF IsNull(uo_selproductor.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	//Close(This)
	lb_Cerrar = False
ELSE
	uo_selproductor.Seleccion(True,True)
END IF

dw_tipoprodu.GetChild("tipr_codigo", idwc_tipopro)
idwc_tipopro.SetTransObject(sqlca)
idwc_tipopro.Retrieve()
dw_tipoprodu.InsertRow(0)

// uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
END IF

// uo_seleccion_variedad
IF IsNull(uo_selvariedad.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selvariedad.Seleccion(True,True)
END IF

dw_pesoneto.GetChild("enva_pesone", idwc_pesoneto)
idwc_pesoneto.SetTransObject(SQLCA)
idwc_pesoneto.Retrieve()
dw_pesoneto.InsertRow(0)
dw_pesoneto.SetItem(1, "enva_pesone", 8.20)
tit_peso.Enabled		=	False
dw_pesoneto.Enabled	=	False
dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))

em_fech_ini.Text			=	String(Relativedate(Today(), -365))
em_fech_fin.Text			=	String(Today())

istr_mant.argumento[1]	= 	String(gi_CodExport) 							// cliente
istr_mant.argumento[2]	=	"-1"													// Zona
istr_mant.argumento[3]	=	"-1"													// Tipo Productor
//istr_mant.argumento[4]  =  "-9"													//	productor
istr_mant.argumento[5]  =  "1"													//	peso
istr_mant.argumento[6]  =  String(Relativedate(Today(), -365))			//	fecha inicial
istr_mant.argumento[7]  =  String(Today())									//	fecha final
istr_mant.argumento[12]	= 	"-9" 													// Calibre

dw_zonas.Object.zona_codigo.BackGround.Color	    = RGB(166,180,210)
dw_tipoprodu.Object.tipr_codigo.BackGround.Color   = RGB(166,180,210)
end event

type st_computador from w_para_informes`st_computador within w_info_produccion_prodzonaembj
end type

type st_usuario from w_para_informes`st_usuario within w_info_produccion_prodzonaembj
end type

type st_temporada from w_para_informes`st_temporada within w_info_produccion_prodzonaembj
end type

type p_logo from w_para_informes`p_logo within w_info_produccion_prodzonaembj
end type

type st_titulo from w_para_informes`st_titulo within w_info_produccion_prodzonaembj
integer width = 1847
string text = "Producción Productor/Embalaje/Zonas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_produccion_prodzonaembj
string tag = "Imprimir Reporte"
integer x = 2313
integer y = 1660
integer taborder = 110
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	ll_Fila, li_cliente, li_tipo = 1, li_tipoprodu, li_zona, li_varirotula
Date		ld_desde, ld_hasta
String	ls_cajas, ls_productor, ls_encabezado, ls_tipoprodu,ls_Calibre, ls_lista
Long		ll_productor
istr_info.titulo	= 'PRODUCCION POR PRODUCTOR/EMBALAJE/ZONAS'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_prodzonaembj_enc"
/*
Especies
*/
IF IsNull(uo_selespecie.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	RETURN
END IF
/*
Variedad
*/
IF IsNull(uo_selvariedad.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	uo_selvariedad.dw_Seleccion.SetFocus()
	RETURN
END IF

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

li_cliente 		=	Integer(istr_mant.argumento[1])
li_zona	 		=	Integer(istr_mant.argumento[2])
li_tipoprodu	=	Integer(istr_mant.argumento[3])
ll_productor	=	Long(istr_mant.argumento[4])
ls_Calibre     =  istr_mant.argumento[12]

IF cbx_peso.Checked	=	False THEN
	ls_cajas = "Bulto"
	istr_mant.argumento[5]	=	"1"
ELSE
	istr_mant.argumento[5]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas						=	"Base " + istr_mant.argumento[5] 
END IF

ld_Desde	=	Date(istr_mant.argumento[6])
ld_Hasta	=	Date(istr_mant.argumento[7])

/*
productor
*/
ls_lista = uo_selproductor.Lista

IF ls_lista = '-9' THEN
	ls_productor = 'Consolidados'
ELSE
	IF ls_lista = '-1' THEN
		ls_productor = 'Todos'
	ELSE
		SELECT prod_nombre INTO:ls_productor
		FROM DBA.productores
		WHERE prod_codigo=:ls_lista;
		ls_productor = String(ls_lista,'00000')+" "+ls_productor
		
		IF ls_productor = '00000' THEN
			ls_productor = ls_lista
		END IF	
		
	END IF
END IF

IF cbx_tipoproductor.checked THEN
		ls_tipoprodu = 'Todos'
ELSE
		SELECT tipr_nombre INTO:ls_tipoprodu
		FROM DBA.tipoproduc
		WHERE tipr_codigo=:li_tipoprodu;
		ls_tipoprodu = String(li_tipoprodu,'000')+" "+ls_tipoprodu
END IF
	
vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_cliente, li_zona, li_tipoprodu,ld_desde,ld_hasta,& 
										 Dec(istr_mant.argumento[5]),uo_selespecie.Codigo,&
										 uo_selvariedad.Codigo,ls_Calibre,li_varirotula,ls_lista)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	F_Membrete(vinf.dw_1)

	vinf.dw_1.Modify("base.text = '" + ls_cajas + "'")
	vinf.dw_1.Modify("tipoprodu.text = '" + ls_tipoprodu + "'")
	vinf.dw_1.Modify("productor.text = '" + ls_productor + "'")
	vinf.dw_1.Modify("desde.text = '" + istr_mant.argumento[6] + "'")
	vinf.dw_1.Modify("hasta.text = '" + istr_mant.argumento[7] + "'")	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_produccion_prodzonaembj
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2313
integer y = 1948
integer taborder = 120
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_produccion_prodzonaembj
integer x = 251
integer y = 440
integer width = 1847
integer height = 832
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_produccion_prodzonaembj
integer x = 823
integer y = 484
integer width = 1152
integer height = 92
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null

SetNull(ls_null)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	
	uo_selproductor.Filtra(Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]),Integer(data))
	
ELSE 
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_produccion_prodzonaembj
integer x = 343
integer y = 512
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_produccion_prodzonaembj
integer x = 343
integer y = 1092
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_produccion_prodzonaembj
integer x = 320
integer y = 1844
integer width = 1705
integer height = 168
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type st_5 from statictext within w_info_produccion_prodzonaembj
integer x = 251
integer y = 1272
integer width = 1847
integer height = 760
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_peso from checkbox within w_info_produccion_prodzonaembj
integer x = 384
integer y = 1904
integer width = 631
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Cajas Equivalentes"
end type

event clicked;IF This.Checked THEN
	tit_peso.Enabled		=	True
	dw_pesoneto.Enabled	=	True
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(rgb(255,255,255)))
ELSE
	tit_peso.Enabled		=	False
	dw_pesoneto.Enabled	=	False
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))
END IF

end event

type dw_pesoneto from datawindow within w_info_produccion_prodzonaembj
integer x = 1280
integer y = 1900
integer width = 695
integer height = 92
integer taborder = 80
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type tit_peso from statictext within w_info_produccion_prodzonaembj
integer x = 1111
integer y = 1912
integer width = 160
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_produccion_prodzonaembj
integer x = 251
integer y = 2032
integer width = 1847
integer height = 164
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_produccion_prodzonaembj
integer x = 343
integer y = 704
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Zona"
boolean focusrectangle = false
end type

type cbx_zonas from checkbox within w_info_produccion_prodzonaembj
integer x = 823
integer y = 600
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_zonas.Enabled											=	False
	dw_zonas.Object.zona_codigo.BackGround.Color		=	RGB(166,180,210)
	istr_mant.argumento[2]	=	'-1'
	
	IF istr_mant.argumento[3]	=	"-1"	THEN
		uo_selproductor.Filtra(-1,integer(istr_mant.argumento[3]),dw_cliente.Object.clie_Codigo[1])
		uo_selproductor.dw_Seleccion.Enabled		=	True
	END IF	
	
ELSE
	dw_zonas.Enabled											=	True
	dw_zonas.Object.zona_codigo.BackGround.Color		=	RGB(255, 255, 255)
	dw_zonas.SetFocus()
END IF
end event

type dw_zonas from datawindow within w_info_produccion_prodzonaembj
integer x = 823
integer y = 676
integer width = 873
integer height = 92
integer taborder = 20
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_zonas"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer li_nula
SetNulL(li_nula)
iuo_zonas = create uo_zonas

IF iuo_zonas.Existe(Integer(Data),True,Sqlca) THEN
istr_mant.argumento[2]	=	data

	IF istr_mant.argumento[3]	=	"-1"	THEN
		uo_selproductor.Filtra(Integer(istr_mant.argumento[2]),-1,dw_cliente.Object.clie_Codigo[1])
		uo_selproductor.dw_Seleccion.Enabled		=	True
	ELSE	
		uo_selproductor.Filtra(Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]),dw_cliente.Object.clie_Codigo[1])
		uo_selproductor.dw_Seleccion.Enabled		=	True
	END IF	
ELSE
	This.SetItem(1,"zona_codigo",li_nula)
END IF
end event

event itemerror;RETURN 1
end event

type st_1 from statictext within w_info_produccion_prodzonaembj
integer x = 343
integer y = 880
integer width = 475
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Tipo Productor"
boolean focusrectangle = false
end type

type cbx_tipoproductor from checkbox within w_info_produccion_prodzonaembj
integer x = 823
integer y = 788
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_tipoprodu.Enabled			= 	False
	dw_tipoprodu.Object.tipr_codigo.BackGround.Color		=	RGB(166,180,210)
	istr_mant.argumento[3] =	"-1"

	IF istr_mant.argumento[2]	=	"-1"	THEN
		uo_selproductor.Filtra(-1,Integer(istr_mant.argumento[2]),dw_cliente.Object.clie_Codigo[1])
		uo_selproductor.dw_Seleccion.Enabled		=	True
	END IF	
	
ELSE
	dw_tipoprodu.Enabled			= 	True
	dw_tipoprodu.Object.tipr_codigo.BackGround.Color		=	RGB(255, 255, 255)
	dw_tipoprodu.SetFocus()
END IF

end event

type dw_tipoprodu from datawindow within w_info_produccion_prodzonaembj
integer x = 823
integer y = 864
integer width = 873
integer height = 92
integer taborder = 30
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_tipoproduc"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_nula
SetNull(li_nula)
iuo_tipoproductor = Create uo_tipoproductor
IF iuo_tipoproductor.Existe(Integer(Data),True,Sqlca) THEN
	istr_mant.argumento[3]=data
	
	IF istr_mant.argumento[2]	=	"-1"	THEN
		uo_selproductor.Filtra(-1,Integer(Data),dw_cliente.Object.clie_Codigo[1])
		uo_selproductor.dw_Seleccion.Enabled		=	True
	ELSE	
		uo_selproductor.Filtra(Integer(istr_mant.argumento[2]),Integer(Data),dw_cliente.Object.clie_Codigo[1])
		uo_selproductor.dw_Seleccion.Enabled		=	True
	END IF	
	
ELSE
	This.SetItem(1,"prod_codigo",li_nula)
END IF

end event

type st_2 from statictext within w_info_produccion_prodzonaembj
integer x = 398
integer y = 2092
integer width = 229
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fech_ini from editmask within w_info_produccion_prodzonaembj
integer x = 709
integer y = 2064
integer width = 393
integer height = 92
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[6]=this.text
end event

type st_3 from statictext within w_info_produccion_prodzonaembj
integer x = 1211
integer y = 2092
integer width = 283
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Término"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fech_fin from editmask within w_info_produccion_prodzonaembj
integer x = 1513
integer y = 2064
integer width = 393
integer height = 92
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[7]=this.text
end event

type st_10 from statictext within w_info_produccion_prodzonaembj
integer x = 343
integer y = 1388
integer width = 421
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_produccion_prodzonaembj
integer x = 338
integer y = 1568
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type st_calidad from statictext within w_info_produccion_prodzonaembj
integer x = 343
integer y = 1772
integer width = 256
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Calidad"
boolean focusrectangle = false
end type

type em_calidad from editmask within w_info_produccion_prodzonaembj
integer x = 823
integer y = 1760
integer width = 261
integer height = 96
integer taborder = 70
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
	li_cliente	=	Integer(istr_mant.argumento[1])  // Cliente
	li_especie	=	Integer(uo_selespecie.Codigo) // Especie
	li_variedad	=	Integer(uo_selvariedad.Codigo) // Variedad
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		istr_mant.argumento[12]	 = iuo_calibre.calibre
		em_calidad.Text			 = iuo_calibre.calibre
		Return 1
	END IF	
END IF	




end event

type cbx_calidad from checkbox within w_info_produccion_prodzonaembj
integer x = 823
integer y = 1676
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_conscalidad.Enabled	=	True
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[12]		=	'-1'
ELSE
	cbx_conscalidad.Enabled	=	False
	cbx_conscalidad.Checked	=	False
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

type cbx_conscalidad from checkbox within w_info_produccion_prodzonaembj
integer x = 1312
integer y = 1680
integer width = 530
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[12]	=	'-9'
ELSE
	istr_mant.argumento[12]	=	'-1'
END IF

end event

type uo_selespecie from uo_seleccion_especie within w_info_produccion_prodzonaembj
event destroy ( )
integer x = 823
integer y = 1292
integer height = 180
integer taborder = 70
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_selvariedad.cbx_Todos.Checked			=	True
		uo_selvariedad.cbx_Consolida.Enabled	=	True
		uo_selvariedad.dw_Seleccion.Enabled		=	False
		
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)
		uo_selvariedad.dw_Seleccion.Enabled		=	True
END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_produccion_prodzonaembj
event destroy ( )
integer x = 823
integer y = 1476
integer taborder = 80
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_produccion_prodzonaembj
integer x = 1723
integer y = 1552
integer width = 334
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Var. Rot."
end type

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_produccion_prodzonaembj
event destroy ( )
integer x = 827
integer y = 968
integer taborder = 140
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

event ue_cambio;call super::ue_cambio;uo_selproductor.Filtra(-1,-1,dw_cliente.Object.clie_Codigo[1])
end event

