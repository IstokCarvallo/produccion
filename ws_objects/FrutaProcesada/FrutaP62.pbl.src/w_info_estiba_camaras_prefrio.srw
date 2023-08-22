$PBExportHeader$w_info_estiba_camaras_prefrio.srw
forward
global type w_info_estiba_camaras_prefrio from w_para_informes
end type
type st_4 from statictext within w_info_estiba_camaras_prefrio
end type
type st_1 from statictext within w_info_estiba_camaras_prefrio
end type
type dw_cliente from datawindow within w_info_estiba_camaras_prefrio
end type
type st_6 from statictext within w_info_estiba_camaras_prefrio
end type
type dw_planta from datawindow within w_info_estiba_camaras_prefrio
end type
type st_3 from statictext within w_info_estiba_camaras_prefrio
end type
type dw_especie from datawindow within w_info_estiba_camaras_prefrio
end type
type st_8 from statictext within w_info_estiba_camaras_prefrio
end type
type dw_camara from datawindow within w_info_estiba_camaras_prefrio
end type
type tit_peso from statictext within w_info_estiba_camaras_prefrio
end type
type st_variedad from statictext within w_info_estiba_camaras_prefrio
end type
type cbx_variedad from checkbox within w_info_estiba_camaras_prefrio
end type
type em_variedad from editmask within w_info_estiba_camaras_prefrio
end type
type cb_buscavariedad from commandbutton within w_info_estiba_camaras_prefrio
end type
type sle_variedad from singlelineedit within w_info_estiba_camaras_prefrio
end type
type st_5 from statictext within w_info_estiba_camaras_prefrio
end type
type cbx_calibre from checkbox within w_info_estiba_camaras_prefrio
end type
type em_calidad from editmask within w_info_estiba_camaras_prefrio
end type
type st_10 from statictext within w_info_estiba_camaras_prefrio
end type
type cbx_especie from checkbox within w_info_estiba_camaras_prefrio
end type
type st_2 from statictext within w_info_estiba_camaras_prefrio
end type
type em_embalaje from editmask within w_info_estiba_camaras_prefrio
end type
type cb_buscaembalaje from commandbutton within w_info_estiba_camaras_prefrio
end type
type cbx_1 from checkbox within w_info_estiba_camaras_prefrio
end type
type cbx_camaras from checkbox within w_info_estiba_camaras_prefrio
end type
type gb_3 from groupbox within w_info_estiba_camaras_prefrio
end type
type st_7 from statictext within w_info_estiba_camaras_prefrio
end type
type cbx_2 from checkbox within w_info_estiba_camaras_prefrio
end type
type st_9 from statictext within w_info_estiba_camaras_prefrio
end type
type st_11 from statictext within w_info_estiba_camaras_prefrio
end type
type st_12 from statictext within w_info_estiba_camaras_prefrio
end type
type st_13 from statictext within w_info_estiba_camaras_prefrio
end type
type st_14 from statictext within w_info_estiba_camaras_prefrio
end type
type em_responsable from editmask within w_info_estiba_camaras_prefrio
end type
type em_fecha from editmask within w_info_estiba_camaras_prefrio
end type
type em_inicio from editmask within w_info_estiba_camaras_prefrio
end type
type em_termino from editmask within w_info_estiba_camaras_prefrio
end type
type em_giro from editmask within w_info_estiba_camaras_prefrio
end type
type cbx_bloquea from checkbox within w_info_estiba_camaras_prefrio
end type
end forward

global type w_info_estiba_camaras_prefrio from w_para_informes
integer x = 14
integer y = 32
integer width = 2560
integer height = 2036
string title = "Informe Estiba Cámaras"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
st_3 st_3
dw_especie dw_especie
st_8 st_8
dw_camara dw_camara
tit_peso tit_peso
st_variedad st_variedad
cbx_variedad cbx_variedad
em_variedad em_variedad
cb_buscavariedad cb_buscavariedad
sle_variedad sle_variedad
st_5 st_5
cbx_calibre cbx_calibre
em_calidad em_calidad
st_10 st_10
cbx_especie cbx_especie
st_2 st_2
em_embalaje em_embalaje
cb_buscaembalaje cb_buscaembalaje
cbx_1 cbx_1
cbx_camaras cbx_camaras
gb_3 gb_3
st_7 st_7
cbx_2 cbx_2
st_9 st_9
st_11 st_11
st_12 st_12
st_13 st_13
st_14 st_14
em_responsable em_responsable
em_fecha em_fecha
em_inicio em_inicio
em_termino em_termino
em_giro em_giro
cbx_bloquea cbx_bloquea
end type
global w_info_estiba_camaras_prefrio w_info_estiba_camaras_prefrio

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_especie, idwc_camara, idwc_packing, idwc_pesoneto

String is_NomPlanta

uo_calibre	iuo_calibre
end variables

forward prototypes
public function string get_clie_nombre (integer clie_codigo)
public function string get_plde_nombre (integer plde_codigo)
public function boolean existeespecie (integer especie)
public function boolean existepacking (ref string ls_columna)
public function boolean existeproductor (long ll_productor)
end prototypes

public function string get_clie_nombre (integer clie_codigo);string ls_nomcli

SELECT clie_nombre 	
	INTO :ls_nomcli FROM 
	dba.clientesprod WHERE 
	clie_codigo= :clie_codigo;

return ls_nomcli
end function

public function string get_plde_nombre (integer plde_codigo);string ls_nomplde

SELECT plde_nombre 	
	INTO :ls_nomplde FROM 
	dba.plantadesp WHERE 
	plde_codigo= :plde_codigo;

return ls_nomplde

end function

public function boolean existeespecie (integer especie);String		ls_Nombre

SELECT	espe_nombre
	INTO	:ls_Nombre
	FROM	dba.especies
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

public function boolean existepacking (ref string ls_columna);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dba.plantadesp
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

public function boolean existeproductor (long ll_productor);String	ls_Nombre

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
	istr_mant.argumento[6] = String(ll_Productor)	
	RETURN True
END IF
end function

on w_info_estiba_camaras_prefrio.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_3=create st_3
this.dw_especie=create dw_especie
this.st_8=create st_8
this.dw_camara=create dw_camara
this.tit_peso=create tit_peso
this.st_variedad=create st_variedad
this.cbx_variedad=create cbx_variedad
this.em_variedad=create em_variedad
this.cb_buscavariedad=create cb_buscavariedad
this.sle_variedad=create sle_variedad
this.st_5=create st_5
this.cbx_calibre=create cbx_calibre
this.em_calidad=create em_calidad
this.st_10=create st_10
this.cbx_especie=create cbx_especie
this.st_2=create st_2
this.em_embalaje=create em_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.cbx_1=create cbx_1
this.cbx_camaras=create cbx_camaras
this.gb_3=create gb_3
this.st_7=create st_7
this.cbx_2=create cbx_2
this.st_9=create st_9
this.st_11=create st_11
this.st_12=create st_12
this.st_13=create st_13
this.st_14=create st_14
this.em_responsable=create em_responsable
this.em_fecha=create em_fecha
this.em_inicio=create em_inicio
this.em_termino=create em_termino
this.em_giro=create em_giro
this.cbx_bloquea=create cbx_bloquea
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_cliente
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.dw_planta
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.dw_especie
this.Control[iCurrent+8]=this.st_8
this.Control[iCurrent+9]=this.dw_camara
this.Control[iCurrent+10]=this.tit_peso
this.Control[iCurrent+11]=this.st_variedad
this.Control[iCurrent+12]=this.cbx_variedad
this.Control[iCurrent+13]=this.em_variedad
this.Control[iCurrent+14]=this.cb_buscavariedad
this.Control[iCurrent+15]=this.sle_variedad
this.Control[iCurrent+16]=this.st_5
this.Control[iCurrent+17]=this.cbx_calibre
this.Control[iCurrent+18]=this.em_calidad
this.Control[iCurrent+19]=this.st_10
this.Control[iCurrent+20]=this.cbx_especie
this.Control[iCurrent+21]=this.st_2
this.Control[iCurrent+22]=this.em_embalaje
this.Control[iCurrent+23]=this.cb_buscaembalaje
this.Control[iCurrent+24]=this.cbx_1
this.Control[iCurrent+25]=this.cbx_camaras
this.Control[iCurrent+26]=this.gb_3
this.Control[iCurrent+27]=this.st_7
this.Control[iCurrent+28]=this.cbx_2
this.Control[iCurrent+29]=this.st_9
this.Control[iCurrent+30]=this.st_11
this.Control[iCurrent+31]=this.st_12
this.Control[iCurrent+32]=this.st_13
this.Control[iCurrent+33]=this.st_14
this.Control[iCurrent+34]=this.em_responsable
this.Control[iCurrent+35]=this.em_fecha
this.Control[iCurrent+36]=this.em_inicio
this.Control[iCurrent+37]=this.em_termino
this.Control[iCurrent+38]=this.em_giro
this.Control[iCurrent+39]=this.cbx_bloquea
end on

on w_info_estiba_camaras_prefrio.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.dw_especie)
destroy(this.st_8)
destroy(this.dw_camara)
destroy(this.tit_peso)
destroy(this.st_variedad)
destroy(this.cbx_variedad)
destroy(this.em_variedad)
destroy(this.cb_buscavariedad)
destroy(this.sle_variedad)
destroy(this.st_5)
destroy(this.cbx_calibre)
destroy(this.em_calidad)
destroy(this.st_10)
destroy(this.cbx_especie)
destroy(this.st_2)
destroy(this.em_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.cbx_1)
destroy(this.cbx_camaras)
destroy(this.gb_3)
destroy(this.st_7)
destroy(this.cbx_2)
destroy(this.st_9)
destroy(this.st_11)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.st_14)
destroy(this.em_responsable)
destroy(this.em_fecha)
destroy(this.em_inicio)
destroy(this.em_termino)
destroy(this.em_giro)
destroy(this.cbx_bloquea)
end on

event open;x	=	0
y	=	0

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
//dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.SetItem(1, "plde_codigo", gi_CodPlanta)

dw_camara.GetChild("cama_codigo", idwc_camara)
idwc_camara.SetTransObject(SQLCA)
idwc_camara.Retrieve(gi_codplanta,1)
dw_camara.InsertRow(0)

dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
dw_especie.InsertRow(0)
dw_especie.SetItem(1, "espe_codigo", gi_CodEspecie)

em_fecha.Text = String(Date(Now()))
em_inicio.Text = String(Time(Now()))
em_giro.Text = String(Time(Now()))
em_termino.Text = String(Time(Now()))

iuo_calibre   						=	Create uo_calibre

istr_mant.argumento[1]	= 	"-1"  						//String(gi_CodExport) 		// cliente
istr_mant.argumento[2]	= 	String(gi_CodPlanta)  	// planta						//	planta
istr_mant.argumento[3]	=	"-1"							//	camara
istr_mant.argumento[4]	= 	"-1" 							// especie
istr_mant.argumento[5]	=	'-1'                     // variedad
istr_mant.argumento[6]	=	'*'                     // embalaje
istr_mant.argumento[7]	=	'*'                     // calibre




end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_estiba_camaras_prefrio
end type

type st_computador from w_para_informes`st_computador within w_info_estiba_camaras_prefrio
end type

type st_usuario from w_para_informes`st_usuario within w_info_estiba_camaras_prefrio
end type

type st_temporada from w_para_informes`st_temporada within w_info_estiba_camaras_prefrio
end type

type p_logo from w_para_informes`p_logo within w_info_estiba_camaras_prefrio
end type

type st_titulo from w_para_informes`st_titulo within w_info_estiba_camaras_prefrio
integer width = 1838
string text = "Informe Cámara PreFrío"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_estiba_camaras_prefrio
string tag = "Imprimir Reporte"
integer x = 2213
integer y = 1248
integer taborder = 100
end type

event pb_acepta::clicked;Integer	fila, li_cliente, li_planta, li_camara, li_especie, li_variedad
String	ls_embalaje, ls_calibre, ls_data_object

//valida seleccion de camara
If IsNull(istr_mant.argumento[3]) Then
 MessageBox("Atención", "Debe Ingresar Cámara Previamente ",Exclamation!)
	RETURN
End If

li_cliente 		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])
li_camara		=	Integer(istr_mant.argumento[3])
li_especie		=	Integer(istr_mant.argumento[4])
li_variedad		=	Integer(istr_mant.argumento[5])
ls_embalaje		=	istr_mant.argumento[6]
ls_calibre		=	istr_mant.argumento[7]

ls_data_object = "dw_info_listado_camara_prefrio"

SetPointer(HourGlass!)
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = ls_data_object

vinf.dw_1.SetTransObject(sqlca)
fila	=	vinf.dw_1.Retrieve(li_cliente, li_planta, li_camara, li_especie, li_variedad, ls_embalaje, ls_calibre)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy("t_fecha.text = '" + em_fecha.text + "'")
	
	If cbx_bloquea.Checked = False Then
		vinf.dw_1.ModIfy("t_horaini.text = '" + em_inicio.text + "'")
		vinf.dw_1.ModIfy("t_horagiro.text = '" + em_giro.text + "'")
		vinf.dw_1.ModIfy("t_horatermino.text = '" + em_termino.text + "'")
	End If
	
	vinf.dw_1.ModIfy("t_responsable.text = '" + em_responsable.text + "'")
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_estiba_camaras_prefrio
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2208
integer y = 1508
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_estiba_camaras_prefrio
integer x = 247
integer y = 440
integer width = 1847
integer height = 344
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

type st_1 from statictext within w_info_estiba_camaras_prefrio
integer x = 306
integer y = 584
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

type dw_cliente from datawindow within w_info_estiba_camaras_prefrio
integer x = 594
integer y = 468
integer width = 1143
integer height = 104
integer taborder = 10
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null

SetNull(ls_null)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	idwc_planta.Retrieve(1)
	idwc_especie.Retrieve()
	dw_especie.SetItem(1, "espe_codigo", dw_especie.Object.espe_codigo[1])
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF

messagebox("",string(idwc_cliente.Retrieve(gi_CodExport)))
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_estiba_camaras_prefrio
integer x = 306
integer y = 468
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

type dw_planta from datawindow within w_info_estiba_camaras_prefrio
integer x = 594
integer y = 576
integer width = 960
integer height = 108
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantaclie"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[2]	= data

dw_camara.GetChild("cama_codigo", idwc_camara)
idwc_camara.SetTransObject(SQLCA)
idwc_camara.Retrieve(gi_codplanta,1)
dw_camara.InsertRow(0)





end event

type st_3 from statictext within w_info_estiba_camaras_prefrio
integer x = 306
integer y = 844
integer width = 421
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

type dw_especie from datawindow within w_info_estiba_camaras_prefrio
integer x = 594
integer y = 832
integer width = 878
integer height = 100
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExisteEspecie(Integer(data)) THEN
	istr_mant.argumento[4]	=	data
ELSE
	This.SetItem(1, "espe_codigo", gi_CodEspecie)
	istr_mant.argumento[4]	=	String(gi_CodEspecie)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_8 from statictext within w_info_estiba_camaras_prefrio
integer x = 306
integer y = 688
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
string text = "Cámara"
boolean focusrectangle = false
end type

type dw_camara from datawindow within w_info_estiba_camaras_prefrio
integer x = 585
integer y = 672
integer width = 1070
integer height = 92
integer taborder = 30
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_camaras_tipo"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[3]	=	data

if data= "0" then 
//	rb_plano.enabled=FALSE;
//	rb_listado.checked = TRUE;
else	
//   rb_plano.enabled=TRUE;
end if 	

end event

type tit_peso from statictext within w_info_estiba_camaras_prefrio
boolean visible = false
integer x = 562
integer y = 1904
integer width = 183
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_estiba_camaras_prefrio
integer x = 306
integer y = 956
integer width = 279
integer height = 96
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

type cbx_variedad from checkbox within w_info_estiba_camaras_prefrio
integer x = 1783
integer y = 948
integer width = 270
integer height = 80
integer taborder = 50
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
	em_variedad.Enabled			=	False
	cb_buscavariedad.Enabled	=	False
	em_variedad.Text				=	''
	sle_variedad.Text				=	''
	istr_mant.argumento[5]		=	'-1'
	
ELSE
	em_variedad.Enabled			=	True
	cb_buscavariedad.Enabled	=	True
END IF
end event

type em_variedad from editmask within w_info_estiba_camaras_prefrio
integer x = 594
integer y = 940
integer width = 233
integer height = 96
integer taborder = 60
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
string mask = "####"
end type

event modified;Integer		li_especie, li_variedad
String		ls_Nombre

//li_especie	=	Integer(istr_mant.argumento[4]) // Especie
li_especie	=	dw_especie.Object.espe_codigo[1]
li_variedad	=	Integer(This.Text)

SELECT	vari_nombre
	INTO	:ls_Nombre
	FROM	dba.variedades
	WHERE	espe_codigo	=	:li_especie
	AND	vari_codigo	=	:li_variedad;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Variedades")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Variedad no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	sle_variedad.Text			=	ls_nombre
	istr_mant.argumento[5]	=	String(li_variedad)	
END IF
end event

type cb_buscavariedad from commandbutton within w_info_estiba_camaras_prefrio
integer x = 846
integer y = 944
integer width = 96
integer height = 84
integer taborder = 70
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

event clicked;Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[1] // Cliente
lstr_busq.argum[2]	=	istr_mant.argumento[4] // Especie

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[4] = "" THEN
	em_variedad.SetFocus()
ELSE
	em_variedad.Text			=	lstr_busq.argum[4]
	sle_variedad.Text			=	lstr_busq.argum[5]
	istr_mant.argumento[5]	=	lstr_busq.argum[4]
	
END IF

end event

type sle_variedad from singlelineedit within w_info_estiba_camaras_prefrio
integer x = 974
integer y = 940
integer width = 741
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_info_estiba_camaras_prefrio
integer x = 247
integer y = 784
integer width = 1847
integer height = 520
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

type cbx_calibre from checkbox within w_info_estiba_camaras_prefrio
integer x = 974
integer y = 1184
integer width = 270
integer height = 80
integer taborder = 80
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
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[7]	=	'*'
ELSE
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

type em_calidad from editmask within w_info_estiba_camaras_prefrio
integer x = 594
integer y = 1164
integer width = 233
integer height = 96
integer taborder = 90
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
end type

event modified;Integer	li_cliente,	li_especie, li_variedad, li_cantid
String	ls_calibre

IF This.Text <> '' THEN
	
	li_especie	=	dw_especie.Object.espe_codigo[1] // Especie
	//li_variedad	=	Integer(istr_mant.argumento[9]) // Variedad
	li_variedad	=	Integer(em_variedad.text)
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		istr_mant.argumento[7]	 = iuo_calibre.calibre
		em_calidad.Text			 = iuo_calibre.calibre
		Return 1
	END IF	
END IF	




end event

type st_10 from statictext within w_info_estiba_camaras_prefrio
integer x = 306
integer y = 1180
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
string text = "Calibre"
boolean focusrectangle = false
end type

type cbx_especie from checkbox within w_info_estiba_camaras_prefrio
integer x = 1783
integer y = 844
integer width = 270
integer height = 76
integer taborder = 80
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
	dw_especie.Enabled		=	False
	istr_mant.argumento[4]	=	'-1'
ELSE
	istr_mant.argumento[4]	=	'11'
	dw_especie.Enabled		=	True
	dw_especie.SetFocus()
END IF

end event

type st_2 from statictext within w_info_estiba_camaras_prefrio
integer x = 306
integer y = 1068
integer width = 288
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Embalaje"
boolean focusrectangle = false
end type

type em_embalaje from editmask within w_info_estiba_camaras_prefrio
integer x = 594
integer y = 1052
integer width = 233
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
end type

event modified;Integer		li_cliente
String		ls_embalaje, ls_Nombre

li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dba.embalajesprod
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embalaje;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Embalajes")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Embalaje no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	istr_mant.argumento[6]	=	ls_embalaje
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_estiba_camaras_prefrio
integer x = 846
integer y = 1056
integer width = 96
integer height = 84
integer taborder = 80
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

event clicked;Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[1] // Cliente

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
	istr_mant.argumento[6]	=	lstr_busq.argum[2]
END IF
end event

type cbx_1 from checkbox within w_info_estiba_camaras_prefrio
integer x = 974
integer y = 1060
integer width = 270
integer height = 80
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_embalaje.Text			=	''
	em_embalaje.Enabled		=	False
	istr_mant.argumento[6]	=	'*'
	cb_buscaembalaje.Enabled = False;
ELSE
	em_embalaje.Enabled		=	True
	em_embalaje.SetFocus()
	cb_buscaembalaje.Enabled = True;
END IF

end event

type cbx_camaras from checkbox within w_info_estiba_camaras_prefrio
integer x = 1783
integer y = 680
integer width = 270
integer height = 76
integer taborder = 80
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
	dw_camara.Enabled		=	False
	istr_mant.argumento[3]	=	'-1'
ELSE
	dw_camara.Enabled		=	True
	dw_camara.SetFocus()
END IF

end event

type gb_3 from groupbox within w_info_estiba_camaras_prefrio
boolean visible = false
integer x = 183
integer y = 1820
integer width = 1614
integer height = 280
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_7 from statictext within w_info_estiba_camaras_prefrio
integer x = 247
integer y = 1304
integer width = 1847
integer height = 464
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

type cbx_2 from checkbox within w_info_estiba_camaras_prefrio
integer x = 1783
integer y = 472
integer width = 270
integer height = 76
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_cliente.Enabled		=	False
	istr_mant.argumento[1]	=	'-1'
ELSE
	dw_cliente.Enabled		=	True
	dw_cliente.SetFocus()
END IF

end event

type st_9 from statictext within w_info_estiba_camaras_prefrio
integer x = 306
integer y = 1340
integer width = 384
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
string text = "Responsable"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_estiba_camaras_prefrio
integer x = 306
integer y = 1444
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
string text = "Fecha"
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_estiba_camaras_prefrio
integer x = 306
integer y = 1548
integer width = 329
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
string text = "Hora Inicio"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_estiba_camaras_prefrio
integer x = 1230
integer y = 1548
integer width = 293
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
string text = "Hora Giro"
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_estiba_camaras_prefrio
integer x = 306
integer y = 1652
integer width = 411
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
string text = "Hora Término"
boolean focusrectangle = false
end type

type em_responsable from editmask within w_info_estiba_camaras_prefrio
integer x = 709
integer y = 1324
integer width = 1371
integer height = 96
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
boolean autoskip = true
end type

type em_fecha from editmask within w_info_estiba_camaras_prefrio
integer x = 709
integer y = 1428
integer width = 425
integer height = 96
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean autoskip = true
end type

type em_inicio from editmask within w_info_estiba_camaras_prefrio
integer x = 709
integer y = 1532
integer width = 425
integer height = 96
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = timemask!
string mask = "hh:mm:ss"
boolean autoskip = true
end type

type em_termino from editmask within w_info_estiba_camaras_prefrio
integer x = 709
integer y = 1636
integer width = 425
integer height = 96
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = timemask!
string mask = "hh:mm:ss"
boolean autoskip = true
end type

type em_giro from editmask within w_info_estiba_camaras_prefrio
integer x = 1568
integer y = 1532
integer width = 425
integer height = 96
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = timemask!
string mask = "hh:mm:ss"
boolean autoskip = true
end type

type cbx_bloquea from checkbox within w_info_estiba_camaras_prefrio
integer x = 1568
integer y = 1652
integer width = 471
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
string text = "Oculta Hora"
end type

