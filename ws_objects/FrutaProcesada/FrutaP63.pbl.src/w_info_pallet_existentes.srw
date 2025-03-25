$PBExportHeader$w_info_pallet_existentes.srw
forward
global type w_info_pallet_existentes from w_para_informes
end type
type st_1 from statictext within w_info_pallet_existentes
end type
type dw_cliente from datawindow within w_info_pallet_existentes
end type
type st_6 from statictext within w_info_pallet_existentes
end type
type dw_planta from datawindow within w_info_pallet_existentes
end type
type st_3 from statictext within w_info_pallet_existentes
end type
type st_variedad from statictext within w_info_pallet_existentes
end type
type st_productor from statictext within w_info_pallet_existentes
end type
type cbx_productor from checkbox within w_info_pallet_existentes
end type
type em_productor from editmask within w_info_pallet_existentes
end type
type cb_buscaproductor from commandbutton within w_info_pallet_existentes
end type
type sle_productor from singlelineedit within w_info_pallet_existentes
end type
type st_2 from statictext within w_info_pallet_existentes
end type
type em_desde from editmask within w_info_pallet_existentes
end type
type st_7 from statictext within w_info_pallet_existentes
end type
type em_hasta from editmask within w_info_pallet_existentes
end type
type cbx_planta from checkbox within w_info_pallet_existentes
end type
type st_12 from statictext within w_info_pallet_existentes
end type
type dw_stat from datawindow within w_info_pallet_existentes
end type
type cbx_consolid from checkbox within w_info_pallet_existentes
end type
type st_55 from statictext within w_info_pallet_existentes
end type
type st_5 from statictext within w_info_pallet_existentes
end type
type uo_selcondicion from uo_seleccion_condicion within w_info_pallet_existentes
end type
type st_8 from statictext within w_info_pallet_existentes
end type
type em_ncajas from editmask within w_info_pallet_existentes
end type
type cbx_pallets from checkbox within w_info_pallet_existentes
end type
type st_9 from statictext within w_info_pallet_existentes
end type
type cbx_ccalidad from checkbox within w_info_pallet_existentes
end type
type uo_selespecie from uo_seleccion_especie within w_info_pallet_existentes
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_pallet_existentes
end type
type cbx_varirotula from checkbox within w_info_pallet_existentes
end type
type uo_selcate from uo_seleccion_categoria within w_info_pallet_existentes
end type
type st_19 from statictext within w_info_pallet_existentes
end type
type st_calidad from statictext within w_info_pallet_existentes
end type
type em_calidad from editmask within w_info_pallet_existentes
end type
type cbx_calidad from checkbox within w_info_pallet_existentes
end type
type cbx_calrot from checkbox within w_info_pallet_existentes
end type
type cbx_consol_cal from checkbox within w_info_pallet_existentes
end type
type st_10 from statictext within w_info_pallet_existentes
end type
type cbx_embarotula from checkbox within w_info_pallet_existentes
end type
type st_4 from statictext within w_info_pallet_existentes
end type
type st_11 from statictext within w_info_pallet_existentes
end type
type dw_etiqueta from datawindow within w_info_pallet_existentes
end type
type cbx_etiqueta from checkbox within w_info_pallet_existentes
end type
type cbx_consetiqueta from checkbox within w_info_pallet_existentes
end type
type uo_selecdestino from uo_seleccion_destinos within w_info_pallet_existentes
end type
type st_13 from statictext within w_info_pallet_existentes
end type
end forward

global type w_info_pallet_existentes from w_para_informes
integer x = 14
integer y = 32
integer width = 3922
integer height = 1880
string title = "Existencias Pallet"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
st_3 st_3
st_variedad st_variedad
st_productor st_productor
cbx_productor cbx_productor
em_productor em_productor
cb_buscaproductor cb_buscaproductor
sle_productor sle_productor
st_2 st_2
em_desde em_desde
st_7 st_7
em_hasta em_hasta
cbx_planta cbx_planta
st_12 st_12
dw_stat dw_stat
cbx_consolid cbx_consolid
st_55 st_55
st_5 st_5
uo_selcondicion uo_selcondicion
st_8 st_8
em_ncajas em_ncajas
cbx_pallets cbx_pallets
st_9 st_9
cbx_ccalidad cbx_ccalidad
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
uo_selcate uo_selcate
st_19 st_19
st_calidad st_calidad
em_calidad em_calidad
cbx_calidad cbx_calidad
cbx_calrot cbx_calrot
cbx_consol_cal cbx_consol_cal
st_10 st_10
cbx_embarotula cbx_embarotula
st_4 st_4
st_11 st_11
dw_etiqueta dw_etiqueta
cbx_etiqueta cbx_etiqueta
cbx_consetiqueta cbx_consetiqueta
uo_selecdestino uo_selecdestino
st_13 st_13
end type
global w_info_pallet_existentes w_info_pallet_existentes

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta,idwc_etiqueta, idwc_stat, idwc_especie, idwc_variedad

uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad
uo_seleccion_destinos 	iuo_selecdestino
uo_calibre					iuo_calibre

end variables

forward prototypes
public function boolean noexistestatus (integer ia_codigo)
public function boolean noexisteetiqueta (integer li_etiqueta)
end prototypes

public function boolean noexistestatus (integer ia_codigo);Integer	li_existe
boolean lb_retorno
SELECT	count(*)
	INTO	:li_existe
	FROM	dbo.status
	WHERE	stat_codigo	= :ia_codigo;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla ClientesProd")
	lb_retorno = TRUE
ELSEIF li_existe > 0 THEN	
	lb_retorno = FALSE
ELSE
	lb_retorno = TRUE
END IF

RETURN lb_retorno
end function

public function boolean noexisteetiqueta (integer li_etiqueta);String	ls_nombre


SELECT	etiq_nombre
INTO    :ls_nombre
FROM	dbo.etiquetas
WHERE	etiq_codigo =  :li_etiqueta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Etiquetas")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Etiqueta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	RETURN False 
END IF
end function

on w_info_pallet_existentes.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_3=create st_3
this.st_variedad=create st_variedad
this.st_productor=create st_productor
this.cbx_productor=create cbx_productor
this.em_productor=create em_productor
this.cb_buscaproductor=create cb_buscaproductor
this.sle_productor=create sle_productor
this.st_2=create st_2
this.em_desde=create em_desde
this.st_7=create st_7
this.em_hasta=create em_hasta
this.cbx_planta=create cbx_planta
this.st_12=create st_12
this.dw_stat=create dw_stat
this.cbx_consolid=create cbx_consolid
this.st_55=create st_55
this.st_5=create st_5
this.uo_selcondicion=create uo_selcondicion
this.st_8=create st_8
this.em_ncajas=create em_ncajas
this.cbx_pallets=create cbx_pallets
this.st_9=create st_9
this.cbx_ccalidad=create cbx_ccalidad
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.uo_selcate=create uo_selcate
this.st_19=create st_19
this.st_calidad=create st_calidad
this.em_calidad=create em_calidad
this.cbx_calidad=create cbx_calidad
this.cbx_calrot=create cbx_calrot
this.cbx_consol_cal=create cbx_consol_cal
this.st_10=create st_10
this.cbx_embarotula=create cbx_embarotula
this.st_4=create st_4
this.st_11=create st_11
this.dw_etiqueta=create dw_etiqueta
this.cbx_etiqueta=create cbx_etiqueta
this.cbx_consetiqueta=create cbx_consetiqueta
this.uo_selecdestino=create uo_selecdestino
this.st_13=create st_13
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.dw_planta
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_variedad
this.Control[iCurrent+7]=this.st_productor
this.Control[iCurrent+8]=this.cbx_productor
this.Control[iCurrent+9]=this.em_productor
this.Control[iCurrent+10]=this.cb_buscaproductor
this.Control[iCurrent+11]=this.sle_productor
this.Control[iCurrent+12]=this.st_2
this.Control[iCurrent+13]=this.em_desde
this.Control[iCurrent+14]=this.st_7
this.Control[iCurrent+15]=this.em_hasta
this.Control[iCurrent+16]=this.cbx_planta
this.Control[iCurrent+17]=this.st_12
this.Control[iCurrent+18]=this.dw_stat
this.Control[iCurrent+19]=this.cbx_consolid
this.Control[iCurrent+20]=this.st_55
this.Control[iCurrent+21]=this.st_5
this.Control[iCurrent+22]=this.uo_selcondicion
this.Control[iCurrent+23]=this.st_8
this.Control[iCurrent+24]=this.em_ncajas
this.Control[iCurrent+25]=this.cbx_pallets
this.Control[iCurrent+26]=this.st_9
this.Control[iCurrent+27]=this.cbx_ccalidad
this.Control[iCurrent+28]=this.uo_selespecie
this.Control[iCurrent+29]=this.uo_selvariedad
this.Control[iCurrent+30]=this.cbx_varirotula
this.Control[iCurrent+31]=this.uo_selcate
this.Control[iCurrent+32]=this.st_19
this.Control[iCurrent+33]=this.st_calidad
this.Control[iCurrent+34]=this.em_calidad
this.Control[iCurrent+35]=this.cbx_calidad
this.Control[iCurrent+36]=this.cbx_calrot
this.Control[iCurrent+37]=this.cbx_consol_cal
this.Control[iCurrent+38]=this.st_10
this.Control[iCurrent+39]=this.cbx_embarotula
this.Control[iCurrent+40]=this.st_4
this.Control[iCurrent+41]=this.st_11
this.Control[iCurrent+42]=this.dw_etiqueta
this.Control[iCurrent+43]=this.cbx_etiqueta
this.Control[iCurrent+44]=this.cbx_consetiqueta
this.Control[iCurrent+45]=this.uo_selecdestino
this.Control[iCurrent+46]=this.st_13
end on

on w_info_pallet_existentes.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.st_variedad)
destroy(this.st_productor)
destroy(this.cbx_productor)
destroy(this.em_productor)
destroy(this.cb_buscaproductor)
destroy(this.sle_productor)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.cbx_planta)
destroy(this.st_12)
destroy(this.dw_stat)
destroy(this.cbx_consolid)
destroy(this.st_55)
destroy(this.st_5)
destroy(this.uo_selcondicion)
destroy(this.st_8)
destroy(this.em_ncajas)
destroy(this.cbx_pallets)
destroy(this.st_9)
destroy(this.cbx_ccalidad)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.uo_selcate)
destroy(this.st_19)
destroy(this.st_calidad)
destroy(this.em_calidad)
destroy(this.cbx_calidad)
destroy(this.cbx_calrot)
destroy(this.cbx_consol_cal)
destroy(this.st_10)
destroy(this.cbx_embarotula)
destroy(this.st_4)
destroy(this.st_11)
destroy(this.dw_etiqueta)
destroy(this.cbx_etiqueta)
destroy(this.cbx_consetiqueta)
destroy(this.uo_selecdestino)
destroy(this.st_13)
end on

event open;call super::open;/* Argumentos :
					 [1] = Cliente
					 [2] = Planta
					 [3] = Especie
					 [4] = Variedad
					 [7] = calidad
*/

x	=	0
y	=	0

Boolean lb_Cerrar

// uo_seleccion_especie
If IsNull(uo_selespecie.Codigo) Then lb_Cerrar = True
If lb_Cerrar Then
	Close(This)
Else
	uo_selespecie.Seleccion(True,True)
End If

iuo_calibre   						=	Create uo_calibre

// uo_seleccion_destino
If IsNull(uo_selecdestino.Codigo) Then lb_Cerrar = True
If lb_Cerrar Then
	Close(This)
Else
	uo_selecdestino.Seleccion(True,True)
End If

If IsNull(uo_SelCate.Codigo) Then lb_Cerrar	=	True
If lb_Cerrar Then
	Close(This)
Else
	uo_SelCate.Seleccion(True, True)
End If

// uo_seleccion_variedad
If IsNull(uo_selvariedad.Codigo) Then lb_Cerrar = True
If lb_Cerrar Then
	Close(This)
Else
	uo_selvariedad.Seleccion(True,True)
	uo_selvariedad.Enabled		=	False
End If

If IsNull(uo_selcondicion.Codigo) Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_selcondicion.Seleccion(True, True)
	uo_selcondicion.cbx_consolida.Visible = FALSE
	uo_selcondicion.cbx_consolida.Enabled = FALSE

	dw_cliente.GetChild("clie_codigo", idwc_cliente)
	idwc_cliente.SetTransObject(SQLCA)
	idwc_cliente.Retrieve()
	dw_cliente.InsertRow(0)
	dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)
	
	dw_planta.GetChild("plde_codigo", idwc_planta)
	idwc_planta.SetTransObject(sqlca)
	idwc_planta.Retrieve(1)
	dw_planta.InsertRow(0)
	dw_planta.SetItem(1, "plde_codigo", gi_CodPlanta)
	
	dw_stat.GetChild("stat_codigo", idwc_stat)
	idwc_stat.SetTransObject(SQLCA)
	idwc_stat.Retrieve()
	dw_stat.InsertRow(0)
	
	dw_etiqueta.GetChild("etiq_codigo", idwc_etiqueta)
	idwc_etiqueta.SetTransObject(sqlca)
	idwc_etiqueta.Retrieve()
	dw_etiqueta.InsertRow(0)
	
	dw_etiqueta.Enabled											=	False
	dw_etiqueta.Object.etiq_codigo.BackGround.Color	=	RGB(166,180,210)

	
	istr_mant.argumento[25]	= "0"
	istr_mant.argumento[26]	= "Consolidado"	
	dw_stat.Object.stat_codigo.background.color = RGB(166,180,210)
	
	em_desde.Text					=	String(RelativeDate(Today(), -365))
	em_hasta.Text					=	String(Today())
	
	istr_mant.argumento[1]		= 	String(gi_CodExport)
	istr_mant.argumento[2]		= 	String(gi_CodPlanta)
	istr_mant.argumento[5]		=	'0'
	istr_mant.argumento[6]		=	'Consolidados'
	istr_mant.argumento[7]		=	'-9'
	istr_mant.argumento[23]	= 	em_desde.Text
	istr_mant.argumento[24]	=	em_hasta.Text
	istr_mant.argumento[27]	= "0"					// Cantidad de cajas por pallets
	istr_mant.argumento[8]  	= "-1"					//	etiqueta
	
	em_ncajas.Enabled				=	False
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_pallet_existentes
integer x = 3538
integer y = 608
end type

type st_computador from w_para_informes`st_computador within w_info_pallet_existentes
end type

type st_usuario from w_para_informes`st_usuario within w_info_pallet_existentes
end type

type st_temporada from w_para_informes`st_temporada within w_info_pallet_existentes
end type

type p_logo from w_para_informes`p_logo within w_info_pallet_existentes
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_pallet_existentes
integer width = 3159
string text = "Resumen De Pallet Existentes"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_pallet_existentes
string tag = "Imprimir Reporte"
integer x = 3598
integer y = 1096
integer taborder = 180
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila,li_planta,li_cliente, li_condicion, li_ncajas, li_ccalidad, li_varirotula, li_calrotula, li_embarotula, li_etiqueta
			
String  	ls_planta,ls_especie, ls_cliente, ls_productor, ls_condicion	
Date		ld_desde, ld_hasta
Long		ll_productor

istr_info.titulo	= 'INFORME DE PALLET EXISTENTES'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_resumen_pallet"
vinf.dw_1.SetTransObject(sqlca)

If cbx_planta.Checked Then
	ls_Planta   =  'Todas'
Else
   ls_planta	=	idwc_planta.GetItemString(idwc_planta.GetRow(), "plde_nombre")
End If

If cbx_consolid.checked Then
	istr_mant.argumento[25]	= "0"
	istr_mant.argumento[26]	= "Consolidado"
End If	

If cbx_ccalidad.checked Then
	li_ccalidad = 1
Else
	li_ccalidad = 0
End If	

//Condición
If uo_selcondicion.cbx_todos.checked Then
   li_condicion	= -1
	ls_condicion   = 'Todas'
Else
   li_condicion	= uo_selcondicion.dw_Seleccion.Object.codigo[1]
	If IsNull(li_condicion)Then
	   MessageBox("Atención","Debe Seleccionar una Condición Previamente",Exclamation!)
	   RETURN
	Else
		ls_condicion   = uo_selcondicion.Nombre
	End If
End If
/*
Especies
*/
If IsNull(uo_selespecie.Codigo)Then
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	RETURN
End If
/*
Categoria
*/
If IsNull(uo_SelCate.Codigo)Then
	MessageBox("Atención","Debe Seleccionar una Categoria Previamente",Exclamation!)
	uo_SelCate.dw_Seleccion.SetFocus()
	RETURN
End If
/*
Variedad
*/
If IsNull(uo_selvariedad.Codigo)Then
	MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	uo_selvariedad.dw_Seleccion.SetFocus()
	RETURN
End If

ll_productor 	= 	Long(istr_mant.argumento[5])
ls_productor	=	istr_mant.argumento[6]

vinf.dw_1.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
idwc_etiqueta.Retrieve()

li_cliente 		=	Integer(istr_mant.argumento[1])

SELECT clie_nombre
INTO   :ls_Cliente
FROM dbo.clientesprod
where clie_codigo = :li_Cliente;

li_planta		=	Integer(istr_mant.argumento[2])
ld_desde			=	Date(istr_mant.argumento[23])
ld_hasta			=	Date(istr_mant.argumento[24])
li_ncajas		=	Integer(istr_mant.argumento[27])
li_etiqueta		=	Integer(istr_mant.argumento[8])

If cbx_varirotula.Checked Then
	li_varirotula = 1
Else
	li_varirotula = 0
End If

If cbx_calrot.Checked Then
	li_calrotula = 1
Else
	li_calrotula = 0
End If

If cbx_embarotula.Checked Then
	li_embarotula = 1
Else
	li_embarotula = 0
End If

/*
destino
*/
If IsNull(uo_selecdestino.Codigo)Then
	MessageBox("Atención","Debe Seleccionar un Destino Previamente",Exclamation!)
	uo_selecdestino.dw_Seleccion.SetFocus()
	RETURN
End If

fila	=	vinf.dw_1.Retrieve(li_cliente, li_planta, uo_selespecie.Codigo,&
			uo_selvariedad.Codigo,ll_productor,ld_desde,ld_hasta,&
			integer(istr_mant.argumento[25]),li_condicion,li_ncajas,li_ccalidad,li_varirotula,&
			uo_SelCate.Codigo,istr_mant.argumento[7],li_calrotula,li_embarotula,li_etiqueta,uo_selecdestino.Codigo)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("planta.text = '" + ls_planta + "'")
		vinf.dw_1.Modify("cliente.text = '" + ls_cliente + "'")
		vinf.dw_1.Modify("t_status.text = '" + Upper(istr_mant.argumento[26]) + "'")
		vinf.dw_1.Modify("productor.text = '" + String(ll_productor,'#####')+' '+ls_productor + "'")
		vinf.dw_1.Modify("desde.text = '" + String(ld_desde)+"'")
		vinf.dw_1.Modify("hasta.text = '" + String(ld_hasta)+"'")	
		vinf.dw_1.Modify("fumigados.text = '" + ls_condicion+"'")	
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_pallet_existentes
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3598
integer y = 1400
integer taborder = 190
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_pallet_existentes
integer x = 270
integer y = 624
integer width = 215
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

type dw_cliente from datawindow within w_info_pallet_existentes
integer x = 567
integer y = 480
integer width = 1147
integer height = 92
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;call super::itemchanged;String	ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
ELSE
	This.SetItem(1, "clie_codigo", gi_CodExport)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_pallet_existentes
integer x = 270
integer y = 508
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

type dw_planta from datawindow within w_info_pallet_existentes
integer x = 567
integer y = 604
integer width = 965
integer height = 96
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2]	=	data
ELSE
	This.SetItem(1, "plde_codigo", gi_CodPlanta)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_info_pallet_existentes
integer x = 270
integer y = 984
integer width = 251
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

type st_variedad from statictext within w_info_pallet_existentes
integer x = 270
integer y = 1168
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

type st_productor from statictext within w_info_pallet_existentes
integer x = 270
integer y = 816
integer width = 297
integer height = 56
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

type cbx_productor from checkbox within w_info_pallet_existentes
integer x = 562
integer y = 716
integer width = 471
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
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_productor.Enabled			=	False
	cb_buscaproductor.Enabled	=	False
	em_productor.Text				=	''
	sle_productor.Text			=	''
	istr_mant.argumento[5]		=	'0'
	istr_mant.argumento[6]		=	'Consolidados'
ELSE
	em_productor.Enabled			=	True
	cb_buscaproductor.Enabled	=	True
END IF
end event

type em_productor from editmask within w_info_pallet_existentes
integer x = 571
integer y = 792
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
string mask = "#####"
end type

event modified;String		ls_Nombre
Long			ll_productor
Integer		li_cliente

IF This.Text <> '' AND This.Text <> '0' THEN
	ll_productor	=	Long(This.Text)
	li_cliente 		=  dw_cliente.Object.clie_codigo[1]
	
	SELECT	prod_nombre
		INTO	:ls_Nombre
		FROM	dbo.productores as pro,dbo.productoresclientes as cli
		WHERE	pro.prod_codigo =	:ll_productor
		AND	pro.prod_codigo = cli.prod_codigo
		AND	:li_cliente in (-1,cli.clie_codigo);
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de tabla Productores")
		This.SetFocus()
	ELSEIF sqlca.SQLCode = 100 THEN
	
		MessageBox("Atención", "Código de productor no ha sido definido o no pertenece a este cliente.~r~r" + &
			"Ingrese o seleccione otro Código.")
		em_productor.Text 	= ''
		sle_productor.Text 	= ''
		This.SetFocus()
	ELSE
		sle_productor.Text			=	ls_Nombre
		istr_mant.argumento[4]		=	String(ll_productor)
		
	   istr_mant.argumento[5]		=	String(ll_productor)
	   istr_mant.argumento[6]		=	ls_Nombre	
		
	END IF
END IF	
end event

type cb_buscaproductor from commandbutton within w_info_pallet_existentes
integer x = 864
integer y = 800
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

OpenWithParm(w_busc_productores_clientes, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[3] = "" THEN
	em_productor.SetFocus()
ELSE
	em_productor.Text 		=	lstr_busq.argum[3]
	sle_productor.Text			=	lstr_busq.argum[4]
	istr_mant.argumento[5]	=	lstr_busq.argum[3]
	istr_mant.argumento[6]	=	lstr_busq.argum[4]
END IF
end event

type sle_productor from singlelineedit within w_info_pallet_existentes
integer x = 974
integer y = 792
integer width = 891
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

type st_2 from statictext within w_info_pallet_existentes
integer x = 334
integer y = 1512
integer width = 192
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
string text = "Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_pallet_existentes
integer x = 558
integer y = 1496
integer width = 421
integer height = 96
integer taborder = 160
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

event modified;istr_mant.argumento[23]	=	This.Text
end event

type st_7 from statictext within w_info_pallet_existentes
integer x = 1088
integer y = 1508
integer width = 270
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
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_pallet_existentes
integer x = 1385
integer y = 1488
integer width = 421
integer height = 96
integer taborder = 170
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

event modified;istr_mant.argumento[24]	=	This.Text
end event

type cbx_planta from checkbox within w_info_pallet_existentes
integer x = 1545
integer y = 608
integer width = 279
integer height = 80
integer taborder = 30
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

event clicked;Integer	li_planta

SetNull(li_planta)
IF This.Checked THEN
	dw_planta.Enabled				=	False
	dw_planta.SetItem(1,"plde_codigo",li_planta)
	istr_mant.argumento[2]		=	'-1'
ELSE
	dw_planta.Enabled				=	True
	dw_planta.SetItem(1, "plde_codigo", gi_CodPlanta)
   istr_mant.argumento[2]	= 	String(gi_CodPlanta)
END IF
end event

type st_12 from statictext within w_info_pallet_existentes
integer x = 1920
integer y = 708
integer width = 265
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
string text = "Status"
boolean focusrectangle = false
end type

type dw_stat from datawindow within w_info_pallet_existentes
integer x = 2235
integer y = 708
integer width = 969
integer height = 88
integer taborder = 130
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_status1"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer li_Null
SetNull(li_Null)

IF  NoExisteStatus(integer(data)) THEN
	MessageBox("informe","No Existe Status")
	dw_stat.SetItem(1,"stat_codigo",li_Null)
	RETURN 1
ELSE	
	istr_mant.argumento[25]	= data
	istr_mant.argumento[26]	=  f_statnombre(integer(data))
END IF

end event

type cbx_consolid from checkbox within w_info_pallet_existentes
integer x = 2231
integer y = 632
integer width = 471
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
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.checked THEN
	dw_stat.enabled = FALSE
	dw_stat.Object.stat_codigo.background.color = RGB(166,180,210)
ELSE
	dw_stat.enabled = TRUE
	dw_stat.Object.stat_codigo.background.color = rgb(255,255,255)
	istr_mant.argumento[25]	= "1"	
	istr_mant.argumento[26]	=  f_statnombre(1)
	dw_stat.Object.stat_codigo[1] = 1
END IF	
end event

type st_55 from statictext within w_info_pallet_existentes
integer x = 242
integer y = 1412
integer width = 1650
integer height = 252
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

type st_5 from statictext within w_info_pallet_existentes
integer x = 1920
integer y = 1060
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
string text = "Condición"
boolean focusrectangle = false
end type

type uo_selcondicion from uo_seleccion_condicion within w_info_pallet_existentes
integer x = 2231
integer y = 980
integer height = 172
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcondicion.destroy
call uo_seleccion_condicion::destroy
end on

type st_8 from statictext within w_info_pallet_existentes
integer x = 1920
integer y = 1176
integer width = 306
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Pallets de "
boolean focusrectangle = false
end type

type em_ncajas from editmask within w_info_pallet_existentes
integer x = 2245
integer y = 1152
integer width = 393
integer height = 96
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "###"
end type

event modified;istr_mant.argumento[27]	=	This.Text
end event

type cbx_pallets from checkbox within w_info_pallet_existentes
integer x = 2761
integer y = 1164
integer width = 279
integer height = 80
integer taborder = 140
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

event clicked;
IF This.Checked THEN
	em_ncajas.Enabled				=	False
	em_ncajas.Text					=	""	
	istr_mant.argumento[27]		=	'0'
ELSE
	em_ncajas.Enabled				=	True
	istr_mant.argumento[27]		=	em_ncajas.Text
	em_ncajas.Setfocus()
END IF


end event

type st_9 from statictext within w_info_pallet_existentes
integer x = 1897
integer y = 1488
integer width = 1504
integer height = 176
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

type cbx_ccalidad from checkbox within w_info_pallet_existentes
integer x = 1957
integer y = 1496
integer width = 763
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
string text = "Por Control de Calidad"
end type

type uo_selespecie from uo_seleccion_especie within w_info_pallet_existentes
event destroy ( )
integer x = 553
integer y = 900
integer height = 180
integer taborder = 50
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
		uo_selvariedad.Enabled						=	False	
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)
		uo_selvariedad.Enabled						=	True
END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_pallet_existentes
event destroy ( )
integer x = 553
integer y = 1088
integer taborder = 60
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_pallet_existentes
integer x = 2715
integer y = 1492
integer width = 667
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
string text = "Variedad Rotulada"
end type

type uo_selcate from uo_seleccion_categoria within w_info_pallet_existentes
integer x = 2226
integer y = 804
integer taborder = 340
boolean bringtotop = true
end type

on uo_selcate.destroy
call uo_seleccion_categoria::destroy
end on

type st_19 from statictext within w_info_pallet_existentes
integer x = 1920
integer y = 864
integer width = 261
integer height = 104
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Categoria"
boolean focusrectangle = false
end type

type st_calidad from statictext within w_info_pallet_existentes
integer x = 279
integer y = 1316
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

type em_calidad from editmask within w_info_pallet_existentes
integer x = 562
integer y = 1292
integer width = 297
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
string mask = "xxxx"
end type

event modified;Integer	li_especie, li_variedad, li_cantid
String	ls_calibre

li_especie	=	Integer(istr_mant.argumento[3]) // Especie
li_variedad	=	Integer(istr_mant.argumento[4]) // Variedad
ls_calibre	=	This.Text


IF This.Text <> '' THEN
	
	li_especie	=	Integer(uo_selespecie.Codigo) // Especie
	li_variedad	=	Integer(uo_selvariedad.Codigo) // Variedad
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

type cbx_calidad from checkbox within w_info_pallet_existentes
integer x = 928
integer y = 1308
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
boolean enabled = false
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[7]	=	'-1'
ELSE
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF

end event

type cbx_calrot from checkbox within w_info_pallet_existentes
integer x = 1957
integer y = 1572
integer width = 718
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
string text = "Calidad Rotulada"
end type

type cbx_consol_cal from checkbox within w_info_pallet_existentes
integer x = 1257
integer y = 1308
integer width = 443
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
string text = "Consolidado"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[7]	=	'-9'
	cbx_calidad.Checked 		=	True
	cbx_calidad.Enabled 		=	False
ELSE
	istr_mant.argumento[7]	=	'-1'
	em_calidad.Enabled		=	False
	cbx_calidad.Enabled 		=	True
	em_calidad.SetFocus()
END IF

end event

type st_10 from statictext within w_info_pallet_existentes
integer x = 1897
integer y = 440
integer width = 1504
integer height = 1044
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

type cbx_embarotula from checkbox within w_info_pallet_existentes
integer x = 2715
integer y = 1572
integer width = 667
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
string text = "Embalaje Rotulado"
end type

type st_4 from statictext within w_info_pallet_existentes
integer x = 242
integer y = 440
integer width = 1650
integer height = 972
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

type st_11 from statictext within w_info_pallet_existentes
integer x = 1920
integer y = 524
integer width = 297
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
string text = "Etiqueta"
boolean focusrectangle = false
end type

type dw_etiqueta from datawindow within w_info_pallet_existentes
integer x = 2231
integer y = 532
integer width = 891
integer height = 96
integer taborder = 80
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_etiquetas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;
IF NoExisteEtiqueta(Integer(data)) THEN
	dw_etiqueta.SetItem(1, "etiq_codigo", 1)
	dw_etiqueta.SetFocus()
	RETURN 1
ELSE
	istr_mant.argumento[8]	=	data
	
END IF


end event

event itemerror;RETURN 1
end event

type cbx_etiqueta from checkbox within w_info_pallet_existentes
integer x = 2231
integer y = 456
integer width = 293
integer height = 72
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
	cbx_consetiqueta.Enabled									=	True
	dw_etiqueta.Enabled										=	False
	dw_etiqueta.Object.etiq_codigo.BackGround.Color	=	RGB(166,180,210)
	istr_mant.argumento[8]									=	'-1'

ELSE
	cbx_consetiqueta.Enabled									=	False
	cbx_consetiqueta.Checked									=	False
	dw_etiqueta.Enabled											=	True
	dw_etiqueta.Object.etiq_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_etiqueta.SetFocus()
	istr_mant.argumento[8]	=	string(dw_etiqueta.Object.Etiq_codigo[1])
END IF
end event

type cbx_consetiqueta from checkbox within w_info_pallet_existentes
integer x = 2647
integer y = 456
integer width = 471
integer height = 72
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
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[8]	=	'-9'
ELSE
	istr_mant.argumento[8]	=	'-1'
END IF

end event

type uo_selecdestino from uo_seleccion_destinos within w_info_pallet_existentes
integer x = 2231
integer y = 1276
integer height = 196
integer taborder = 200
boolean bringtotop = true
end type

on uo_selecdestino.destroy
call uo_seleccion_destinos::destroy
end on

type st_13 from statictext within w_info_pallet_existentes
integer x = 1920
integer y = 1368
integer width = 306
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Destino"
boolean focusrectangle = false
end type

