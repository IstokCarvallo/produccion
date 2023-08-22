$PBExportHeader$w_info_pallet_historico.srw
forward
global type w_info_pallet_historico from w_para_informes
end type
type st_4 from statictext within w_info_pallet_historico
end type
type st_1 from statictext within w_info_pallet_historico
end type
type st_2 from statictext within w_info_pallet_historico
end type
type em_desde from editmask within w_info_pallet_historico
end type
type st_6 from statictext within w_info_pallet_historico
end type
type dw_planta from datawindow within w_info_pallet_historico
end type
type st_3 from statictext within w_info_pallet_historico
end type
type cbx_planta from checkbox within w_info_pallet_historico
end type
type st_7 from statictext within w_info_pallet_historico
end type
type em_hasta from editmask within w_info_pallet_historico
end type
type st_9 from statictext within w_info_pallet_historico
end type
type cbx_packing from checkbox within w_info_pallet_historico
end type
type dw_packing from datawindow within w_info_pallet_historico
end type
type st_variedad from statictext within w_info_pallet_historico
end type
type st_5 from statictext within w_info_pallet_historico
end type
type cbx_calidad from checkbox within w_info_pallet_historico
end type
type em_calidad from editmask within w_info_pallet_historico
end type
type st_10 from statictext within w_info_pallet_historico
end type
type st_11 from statictext within w_info_pallet_historico
end type
type gb_3 from groupbox within w_info_pallet_historico
end type
type cbx_1 from checkbox within w_info_pallet_historico
end type
type cbx_embalaje from checkbox within w_info_pallet_historico
end type
type em_embalaje from editmask within w_info_pallet_historico
end type
type cb_buscaembalaje from commandbutton within w_info_pallet_historico
end type
type st_embalaje from statictext within w_info_pallet_historico
end type
type st_12 from statictext within w_info_pallet_historico
end type
type cbx_todosfru from checkbox within w_info_pallet_historico
end type
type cbx_consfru from checkbox within w_info_pallet_historico
end type
type dw_frurecep from datawindow within w_info_pallet_historico
end type
type dw_pesoneto from datawindow within w_info_pallet_historico
end type
type tit_peso from statictext within w_info_pallet_historico
end type
type uo_selespecie from uo_seleccion_especie within w_info_pallet_historico
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_pallet_historico
end type
type cbx_varirotula from checkbox within w_info_pallet_historico
end type
type st_14 from statictext within w_info_pallet_historico
end type
type rb_apallet from radiobutton within w_info_pallet_historico
end type
type rb_pucho from radiobutton within w_info_pallet_historico
end type
type rb_todospall from radiobutton within w_info_pallet_historico
end type
type st_15 from statictext within w_info_pallet_historico
end type
type cbx_consogene from checkbox within w_info_pallet_historico
end type
type cbx_consofechas from checkbox within w_info_pallet_historico
end type
type cbx_consorden from checkbox within w_info_pallet_historico
end type
type cbx_3 from checkbox within w_info_pallet_historico
end type
type em_norden from editmask within w_info_pallet_historico
end type
type st_21 from statictext within w_info_pallet_historico
end type
type st_16 from statictext within w_info_pallet_historico
end type
type st_8 from statictext within w_info_pallet_historico
end type
type st_13 from statictext within w_info_pallet_historico
end type
type em_guiacontrol from editmask within w_info_pallet_historico
end type
type st_17 from statictext within w_info_pallet_historico
end type
type cbx_consolcalifi from checkbox within w_info_pallet_historico
end type
type cbx_todcalifi from checkbox within w_info_pallet_historico
end type
type ddlb_calificacion from dropdownlistbox within w_info_pallet_historico
end type
type st_29 from statictext within w_info_pallet_historico
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_pallet_historico
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_pallet_historico
end type
type st_18 from statictext within w_info_pallet_historico
end type
type em_desde1 from editmask within w_info_pallet_historico
end type
type em_hasta1 from editmask within w_info_pallet_historico
end type
type st_19 from statictext within w_info_pallet_historico
end type
end forward

global type w_info_pallet_historico from w_para_informes
integer x = 14
integer y = 32
integer width = 3918
integer height = 2168
string title = "Pallets Históricos"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_2 st_2
em_desde em_desde
st_6 st_6
dw_planta dw_planta
st_3 st_3
cbx_planta cbx_planta
st_7 st_7
em_hasta em_hasta
st_9 st_9
cbx_packing cbx_packing
dw_packing dw_packing
st_variedad st_variedad
st_5 st_5
cbx_calidad cbx_calidad
em_calidad em_calidad
st_10 st_10
st_11 st_11
gb_3 gb_3
cbx_1 cbx_1
cbx_embalaje cbx_embalaje
em_embalaje em_embalaje
cb_buscaembalaje cb_buscaembalaje
st_embalaje st_embalaje
st_12 st_12
cbx_todosfru cbx_todosfru
cbx_consfru cbx_consfru
dw_frurecep dw_frurecep
dw_pesoneto dw_pesoneto
tit_peso tit_peso
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
st_14 st_14
rb_apallet rb_apallet
rb_pucho rb_pucho
rb_todospall rb_todospall
st_15 st_15
cbx_consogene cbx_consogene
cbx_consofechas cbx_consofechas
cbx_consorden cbx_consorden
cbx_3 cbx_3
em_norden em_norden
st_21 st_21
st_16 st_16
st_8 st_8
st_13 st_13
em_guiacontrol em_guiacontrol
st_17 st_17
cbx_consolcalifi cbx_consolcalifi
cbx_todcalifi cbx_todcalifi
ddlb_calificacion ddlb_calificacion
st_29 st_29
uo_selproductor uo_selproductor
uo_selcliente uo_selcliente
st_18 st_18
em_desde1 em_desde1
em_hasta1 em_hasta1
st_19 st_19
end type
global w_info_pallet_historico w_info_pallet_historico

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_productor, idwc_packing,&
						idwc_pesoneto, idwc_fruta

String is_NomPlanta
Long	ll_norden
Integer	ii_calificacion

uo_frutarecepcion   					iuo_frutarecepcion
uo_seleccion_especie					iuo_selespecie
uo_seleccion_variedad				iuo_selvariedad
uo_seleccion_varios_productores	iuo_selproductor
uo_calibre								iuo_calibre
end variables

forward prototypes
public function boolean existeespecie (integer especie)
public function boolean existeproductor (long ll_productor)
public function boolean existepacking (integer li_planta)
public function string buscdescfruta (integer codigo)
end prototypes

public function boolean existeespecie (integer especie);String		ls_Nombre

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

public function boolean existeproductor (long ll_productor);String	ls_Nombre

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

public function boolean existepacking (integer li_planta);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dbo.plantadesp
WHERE	plde_codigo =  :li_planta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Planta")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[7] = String(li_planta)	
	RETURN True 
END IF
end function

public function string buscdescfruta (integer codigo);String	ls_descri


  SELECT frre_descri  
    INTO :ls_descri  
    FROM dbo.recfruprocee as re, dbo.frutarecibida as fr 
   WHERE re.frre_codigo = fr.frre_codigo and  
         re.frre_codigo = :codigo ;

		
RETURN ls_descri

end function

on w_info_pallet_historico.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_3=create st_3
this.cbx_planta=create cbx_planta
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_9=create st_9
this.cbx_packing=create cbx_packing
this.dw_packing=create dw_packing
this.st_variedad=create st_variedad
this.st_5=create st_5
this.cbx_calidad=create cbx_calidad
this.em_calidad=create em_calidad
this.st_10=create st_10
this.st_11=create st_11
this.gb_3=create gb_3
this.cbx_1=create cbx_1
this.cbx_embalaje=create cbx_embalaje
this.em_embalaje=create em_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.st_embalaje=create st_embalaje
this.st_12=create st_12
this.cbx_todosfru=create cbx_todosfru
this.cbx_consfru=create cbx_consfru
this.dw_frurecep=create dw_frurecep
this.dw_pesoneto=create dw_pesoneto
this.tit_peso=create tit_peso
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.st_14=create st_14
this.rb_apallet=create rb_apallet
this.rb_pucho=create rb_pucho
this.rb_todospall=create rb_todospall
this.st_15=create st_15
this.cbx_consogene=create cbx_consogene
this.cbx_consofechas=create cbx_consofechas
this.cbx_consorden=create cbx_consorden
this.cbx_3=create cbx_3
this.em_norden=create em_norden
this.st_21=create st_21
this.st_16=create st_16
this.st_8=create st_8
this.st_13=create st_13
this.em_guiacontrol=create em_guiacontrol
this.st_17=create st_17
this.cbx_consolcalifi=create cbx_consolcalifi
this.cbx_todcalifi=create cbx_todcalifi
this.ddlb_calificacion=create ddlb_calificacion
this.st_29=create st_29
this.uo_selproductor=create uo_selproductor
this.uo_selcliente=create uo_selcliente
this.st_18=create st_18
this.em_desde1=create em_desde1
this.em_hasta1=create em_hasta1
this.st_19=create st_19
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.dw_planta
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.cbx_planta
this.Control[iCurrent+9]=this.st_7
this.Control[iCurrent+10]=this.em_hasta
this.Control[iCurrent+11]=this.st_9
this.Control[iCurrent+12]=this.cbx_packing
this.Control[iCurrent+13]=this.dw_packing
this.Control[iCurrent+14]=this.st_variedad
this.Control[iCurrent+15]=this.st_5
this.Control[iCurrent+16]=this.cbx_calidad
this.Control[iCurrent+17]=this.em_calidad
this.Control[iCurrent+18]=this.st_10
this.Control[iCurrent+19]=this.st_11
this.Control[iCurrent+20]=this.gb_3
this.Control[iCurrent+21]=this.cbx_1
this.Control[iCurrent+22]=this.cbx_embalaje
this.Control[iCurrent+23]=this.em_embalaje
this.Control[iCurrent+24]=this.cb_buscaembalaje
this.Control[iCurrent+25]=this.st_embalaje
this.Control[iCurrent+26]=this.st_12
this.Control[iCurrent+27]=this.cbx_todosfru
this.Control[iCurrent+28]=this.cbx_consfru
this.Control[iCurrent+29]=this.dw_frurecep
this.Control[iCurrent+30]=this.dw_pesoneto
this.Control[iCurrent+31]=this.tit_peso
this.Control[iCurrent+32]=this.uo_selespecie
this.Control[iCurrent+33]=this.uo_selvariedad
this.Control[iCurrent+34]=this.cbx_varirotula
this.Control[iCurrent+35]=this.st_14
this.Control[iCurrent+36]=this.rb_apallet
this.Control[iCurrent+37]=this.rb_pucho
this.Control[iCurrent+38]=this.rb_todospall
this.Control[iCurrent+39]=this.st_15
this.Control[iCurrent+40]=this.cbx_consogene
this.Control[iCurrent+41]=this.cbx_consofechas
this.Control[iCurrent+42]=this.cbx_consorden
this.Control[iCurrent+43]=this.cbx_3
this.Control[iCurrent+44]=this.em_norden
this.Control[iCurrent+45]=this.st_21
this.Control[iCurrent+46]=this.st_16
this.Control[iCurrent+47]=this.st_8
this.Control[iCurrent+48]=this.st_13
this.Control[iCurrent+49]=this.em_guiacontrol
this.Control[iCurrent+50]=this.st_17
this.Control[iCurrent+51]=this.cbx_consolcalifi
this.Control[iCurrent+52]=this.cbx_todcalifi
this.Control[iCurrent+53]=this.ddlb_calificacion
this.Control[iCurrent+54]=this.st_29
this.Control[iCurrent+55]=this.uo_selproductor
this.Control[iCurrent+56]=this.uo_selcliente
this.Control[iCurrent+57]=this.st_18
this.Control[iCurrent+58]=this.em_desde1
this.Control[iCurrent+59]=this.em_hasta1
this.Control[iCurrent+60]=this.st_19
end on

on w_info_pallet_historico.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.cbx_planta)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_9)
destroy(this.cbx_packing)
destroy(this.dw_packing)
destroy(this.st_variedad)
destroy(this.st_5)
destroy(this.cbx_calidad)
destroy(this.em_calidad)
destroy(this.st_10)
destroy(this.st_11)
destroy(this.gb_3)
destroy(this.cbx_1)
destroy(this.cbx_embalaje)
destroy(this.em_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.st_embalaje)
destroy(this.st_12)
destroy(this.cbx_todosfru)
destroy(this.cbx_consfru)
destroy(this.dw_frurecep)
destroy(this.dw_pesoneto)
destroy(this.tit_peso)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.st_14)
destroy(this.rb_apallet)
destroy(this.rb_pucho)
destroy(this.rb_todospall)
destroy(this.st_15)
destroy(this.cbx_consogene)
destroy(this.cbx_consofechas)
destroy(this.cbx_consorden)
destroy(this.cbx_3)
destroy(this.em_norden)
destroy(this.st_21)
destroy(this.st_16)
destroy(this.st_8)
destroy(this.st_13)
destroy(this.em_guiacontrol)
destroy(this.st_17)
destroy(this.cbx_consolcalifi)
destroy(this.cbx_todcalifi)
destroy(this.ddlb_calificacion)
destroy(this.st_29)
destroy(this.uo_selproductor)
destroy(this.uo_selcliente)
destroy(this.st_18)
destroy(this.em_desde1)
destroy(this.em_hasta1)
destroy(this.st_19)
end on

event open;Boolean lb_Cerrar

x	=	0
y	=	0

iuo_frutarecepcion	=	Create uo_frutarecepcion   
iuo_calibre   		=	Create uo_calibre

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)

dw_packing.GetChild("plde_codigo", idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve( 2)
dw_packing.InsertRow(0)

IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelEspecie.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelProductor.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelVariedad.Codigo) THEN lb_Cerrar = True

// uo_seleccion_especie

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelEspecie.Seleccion(True,True)
	uo_SelProductor.Seleccion(True,False)
	uo_SelVariedad.Seleccion(True,True)
	uo_SelCliente.Seleccion(True,False)
END IF

dw_pesoneto.GetChild("enva_pesone", idwc_pesoneto)
idwc_pesoneto.SetTransObject(SQLCA)
idwc_pesoneto.Retrieve(0)
dw_pesoneto.InsertRow(0)
dw_pesoneto.SetItem(1, "enva_pesone", 820/100)
tit_peso.Enabled		=	False
dw_pesoneto.Enabled	=	False
dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))

dw_frurecep.GetChild("frre_codigo", idwc_fruta)
idwc_fruta.SetTransObject(SQLCA)
idwc_fruta.Retrieve()
dw_frurecep.InsertRow(0)

dw_frurecep.enabled	=	false

em_Desde.Text				=	String(RelativeDate(Today(), -365))
em_Hasta.Text				=	String(Today())
em_Desde1.Text			=	String(RelativeDate(Today(), -365))
em_Hasta1.Text			=	String(Today())

istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
istr_mant.argumento[2]	= 	"0"							//	planta
istr_mant.argumento[3]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[4]	=	em_hasta.Text				//	fecha final
istr_mant.argumento[7]  =  "0"								//	packing
istr_mant.argumento[8]  =  "1"							//	peso
istr_mant.argumento[28]	=	'-1'
ll_norden					=	-9								// Orden de proceso
ii_calificacion 			= 	-9                      // Calificacion
end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_pallet_historico
integer x = 3470
integer y = 804
end type

type st_computador from w_para_informes`st_computador within w_info_pallet_historico
end type

type st_usuario from w_para_informes`st_usuario within w_info_pallet_historico
end type

type st_temporada from w_para_informes`st_temporada within w_info_pallet_historico
end type

type p_logo from w_para_informes`p_logo within w_info_pallet_historico
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_pallet_historico
integer width = 3072
string text = "Listado de Pallets Historicos (Embalaje)"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_pallet_historico
string tag = "Imprimir Reporte"
integer x = 3506
integer y = 1152
integer taborder = 220
end type

event pb_acepta::clicked;Integer	fila, li_cliente, li_planta, li_fruta, li_varirotula = 0,li_tipallet, li_consogene = 0, li_consofecha = 0
Date		ld_desde, ld_hasta, ld_desde1, ld_hasta1
String	texto_desde, texto_hasta, texto_fecha, ls_nroguia, ls_cajas, ls_descri,ls_tipallet, ls_lista
Long		ll_guiacontrol

istr_info.titulo	= 'LISTADO DE PALLETS HISTORICOS'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_pallet_historico"

ll_guiacontrol	=	Long(em_guiacontrol.Text)
If IsNull(ll_guiacontrol) OR ll_guiacontrol = 0 Then ll_guiacontrol	=	-1
/*
Especies
*/
If IsNull(uo_selespecie.Codigo)Then
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	Return
End If
/*
Variedad
*/
If IsNull(uo_selvariedad.Codigo)Then
	MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	uo_selvariedad.dw_Seleccion.SetFocus()
	Return
End If

If cbx_varirotula.Checked Then  li_varirotula = 1
If cbx_consofechas.Checked Then li_consofecha = 1
If cbx_consogene.checked Then li_consogene = 1
/*
productor
*/
ls_lista = uo_selproductor.Lista

li_planta			=	Integer(istr_mant.argumento[2])
ld_desde			=	Date(istr_mant.argumento[3])
ld_hasta			=	Date(istr_mant.argumento[4])
texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta
ls_nroguia		=	"Todas"

If cbx_1.Checked=False Then
	ls_cajas = "Reales"
	istr_mant.argumento[8]	=	"1"
Else
	istr_mant.argumento[8]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas = "Base " + istr_mant.argumento[8] 
End If
If cbx_embalaje.checked Then
	istr_mant.argumento[29] = "z"
End If

//Caracteristica de Recepción
If cbx_consfru.Checked Then
	li_fruta 	= -9
	ls_descri	= 'CONSOLIDADA'
ElseIf cbx_todosfru.checked  Then
	li_fruta  =   -1
	ls_descri	=	'TODAS'
Else
	li_fruta	=	dw_frurecep.Object.frre_codigo[1]
	If IsNull(li_fruta) OR li_fruta = 0 Then
	   MessageBox("Atención","Debe Seleccionar una Carácteristica Previamente",Exclamation!)
		dw_frurecep.setfocus()
		Return
	Else
		ls_descri	=	buscdescfruta(li_fruta)
	End If 
End If

If rb_apallet.checked Then
	li_tipallet = 1
	ls_tipallet = 'Completos'
End If

If rb_pucho.checked Then
	li_tipallet = 2
	ls_tipallet = 'Puchos'	
End If

If rb_todospall.checked Then
	li_tipallet = -1
	ls_tipallet = 'Todos'	
End If

ld_Desde1= Date(em_Desde1.Text)
ld_Hasta1 = Date(em_Hasta1.Text)

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, li_planta, ld_desde, ld_hasta, uo_selespecie.Codigo,&
								0,Integer(istr_mant.argumento[7]),&
								Dec(istr_mant.argumento[8]),uo_selvariedad.Codigo,&
								istr_mant.argumento[28],istr_mant.argumento[29], li_fruta,li_varirotula,&
								li_tipallet,li_consogene,li_consofecha,ll_norden,ls_lista,ll_guiacontrol,ii_calIficacion,ld_desde1, ld_hasta1)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	//		vinf.dw_1.Object.titulo_informe.text = 'Pallets Historicos'
	//		vinf.dw_1.ModIfy("guia.text = '" + ls_nroguia + "'")
	vinf.dw_1.ModIfy("fechas.text = '" + texto_fecha + "'")
	vinf.dw_1.ModIfy("fruta.text = '" + ls_descri + "'")
	//		vinf.dw_1.ModIfy("Cajas.text = '" + ls_cajas + "'")
	vinf.dw_1.ModIfy("tipallet.text = '" + ls_tipallet + "'")
	vinf.dw_1.ModIfy("t_guiacontrol.text = '" + String(ll_guiacontrol) + "'")	
	vinf.dw_1.ModIfy('DataWindow.Zoom = 88')
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If		
end event

type pb_salir from w_para_informes`pb_salir within w_info_pallet_historico
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3502
integer y = 1440
integer taborder = 230
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_pallet_historico
integer x = 251
integer y = 440
integer width = 1573
integer height = 880
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

type st_1 from statictext within w_info_pallet_historico
integer x = 283
integer y = 716
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

type st_2 from statictext within w_info_pallet_historico
integer x = 279
integer y = 1352
integer width = 320
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
string text = "Embalaje"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_pallet_historico
integer x = 626
integer y = 1340
integer width = 398
integer height = 96
integer taborder = 200
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

type st_6 from statictext within w_info_pallet_historico
integer x = 283
integer y = 564
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

type dw_planta from datawindow within w_info_pallet_historico
integer x = 594
integer y = 712
integer width = 969
integer height = 92
integer taborder = 50
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null
Integer	li_Cliente

SetNull(ls_null)

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExistePlanta(li_cliente,Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2]	=	data
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_info_pallet_historico
integer x = 1865
integer y = 540
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

type cbx_planta from checkbox within w_info_pallet_historico
integer x = 599
integer y = 636
integer width = 402
integer height = 76
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
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_planta.Enabled			=	False
	istr_mant.argumento[2]	=	'0'
ELSE
	dw_planta.Enabled			=	True
	dw_planta.SetFocus()
END IF
end event

type st_7 from statictext within w_info_pallet_historico
integer x = 1038
integer y = 1460
integer width = 105
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
string text = "-"
alignment alignment = center!
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_pallet_historico
integer x = 1166
integer y = 1336
integer width = 402
integer height = 96
integer taborder = 210
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

event modified;istr_mant.argumento[4]	=	This.Text
end event

type st_9 from statictext within w_info_pallet_historico
integer x = 283
integer y = 1200
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

type cbx_packing from checkbox within w_info_pallet_historico
integer x = 599
integer y = 1120
integer width = 402
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

event clicked;IF This.checked = True THEN
	dw_packing.Enabled = False
	istr_mant.argumento[7]	=	'0'
ELSE
	dw_packing.Enabled=True	
END IF	
end event

type dw_packing from datawindow within w_info_pallet_historico
integer x = 594
integer y = 1200
integer width = 965
integer height = 100
integer taborder = 110
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	 ls_null
SetNull(ls_null)

	IF ExistePacking(Integer(data))THEN
		istr_mant.argumento[7]	=	data		
		RETURN 0
	ELSE
		This.SetItem(1, "plde_codigo", ls_null)
		RETURN 1
	END IF
end event

type st_variedad from statictext within w_info_pallet_historico
integer x = 1861
integer y = 768
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

type st_5 from statictext within w_info_pallet_historico
integer x = 1829
integer y = 436
integer width = 1495
integer height = 1116
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

type cbx_calidad from checkbox within w_info_pallet_historico
integer x = 2176
integer y = 972
integer width = 297
integer height = 80
integer taborder = 170
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
	istr_mant.argumento[28]	=	'-1'
ELSE
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

type em_calidad from editmask within w_info_pallet_historico
integer x = 2176
integer y = 1048
integer width = 261
integer height = 96
integer taborder = 190
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
string mask = "xxxxx"
end type

event modified;Integer	li_especie, li_variedad, li_cantid
String	ls_calibre

IF This.Text <> '' THEN
	
	li_especie	=	Integer(uo_selespecie.Codigo) // Especie
	li_variedad	=	Integer(uo_selvariedad.Codigo) // Variedad
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		istr_mant.argumento[28]	 = iuo_calibre.calibre
		em_calidad.Text			 = iuo_calibre.calibre
		Return 1
	END IF	
END IF	

end event

type st_10 from statictext within w_info_pallet_historico
integer x = 1865
integer y = 1048
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

type st_11 from statictext within w_info_pallet_historico
integer x = 251
integer y = 1324
integer width = 1573
integer height = 228
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

type gb_3 from groupbox within w_info_pallet_historico
boolean visible = false
integer x = 1120
integer y = 2228
integer width = 1614
integer height = 280
integer taborder = 240
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type cbx_1 from checkbox within w_info_pallet_historico
boolean visible = false
integer x = 3214
integer y = 2064
integer width = 631
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
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

type cbx_embalaje from checkbox within w_info_pallet_historico
integer x = 2880
integer y = 972
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
	istr_mant.argumento[29]		=	'z'
ELSE
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
END IF
end event

type em_embalaje from editmask within w_info_pallet_historico
integer x = 2880
integer y = 1048
integer width = 297
integer height = 96
integer taborder = 140
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

event modified;Integer		li_cliente
String		ls_embalaje, ls_Nombre

li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dbo.embalajesprod
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
	istr_mant.argumento[29]	=	ls_embalaje
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_pallet_historico
integer x = 3191
integer y = 1056
integer width = 96
integer height = 84
integer taborder = 160
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

lstr_busq.argum[1]	=	istr_mant.argumento[1] // Cliente

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
	istr_mant.argumento[29]	=	lstr_busq.argum[2]
END IF
end event

type st_embalaje from statictext within w_info_pallet_historico
integer x = 2592
integer y = 992
integer width = 288
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
string text = "Embalaje"
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_pallet_historico
integer x = 1856
integer y = 1364
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

type cbx_todosfru from checkbox within w_info_pallet_historico
integer x = 2409
integer y = 1172
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

type cbx_consfru from checkbox within w_info_pallet_historico
integer x = 2720
integer y = 1172
integer width = 443
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

type dw_frurecep from datawindow within w_info_pallet_historico
integer x = 2409
integer y = 1340
integer width = 567
integer height = 104
integer taborder = 90
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
END IF
end event

type dw_pesoneto from datawindow within w_info_pallet_historico
boolean visible = false
integer x = 1221
integer y = 2104
integer width = 544
integer height = 84
integer taborder = 20
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type tit_peso from statictext within w_info_pallet_historico
boolean visible = false
integer x = 1029
integer y = 2124
integer width = 183
integer height = 64
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

type uo_selespecie from uo_seleccion_especie within w_info_pallet_historico
event destroy ( )
integer x = 2176
integer y = 452
integer height = 192
integer taborder = 100
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

END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_pallet_historico
event destroy ( )
integer x = 2176
integer y = 692
integer taborder = 120
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_pallet_historico
integer x = 2185
integer y = 864
integer width = 741
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

event clicked;IF This.Checked THEN
	uo_Selvariedad.cbx_todos.checked	= True
	uo_Selvariedad.Enabled				=	False
ELSE
	uo_Selvariedad.Enabled				=	True	
END IF
end event

type st_14 from statictext within w_info_pallet_historico
integer x = 251
integer y = 1856
integer width = 3072
integer height = 100
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

type rb_apallet from radiobutton within w_info_pallet_historico
integer x = 1248
integer y = 1872
integer width = 283
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
string text = "Pallet"
end type

event clicked;istr_mant.argumento[33]	=	'1'
end event

type rb_pucho from radiobutton within w_info_pallet_historico
integer x = 1742
integer y = 1872
integer width = 302
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
string text = "Puchos"
end type

event clicked;istr_mant.argumento[33]	=	'2'
end event

type rb_todospall from radiobutton within w_info_pallet_historico
integer x = 2254
integer y = 1872
integer width = 370
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
string text = "Todos"
boolean checked = true
end type

event clicked;istr_mant.argumento[33]	=	'-1'
end event

type st_15 from statictext within w_info_pallet_historico
integer x = 251
integer y = 1956
integer width = 3072
integer height = 96
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

type cbx_consogene from checkbox within w_info_pallet_historico
integer x = 549
integer y = 1968
integer width = 1147
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
string text = "Consolida Características Generales "
end type

event clicked;IF This.Checked THEN
	cbx_consofechas.Enabled	=	True
ELSE
	cbx_consofechas.Enabled	=	False
END IF
end event

type cbx_consofechas from checkbox within w_info_pallet_historico
integer x = 1861
integer y = 1968
integer width = 731
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
string text = "Consolida Fechas"
end type

type cbx_consorden from checkbox within w_info_pallet_historico
integer x = 1673
integer y = 1588
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
string text = "Consolidada"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_norden.Enabled = False
	em_norden.Text = ''
	ll_norden = -9
	cbx_3.Checked = True
	cbx_3.Enabled = False
ELSE
	em_norden.Enabled = False
	ll_norden = -1
	cbx_3.Checked = True
	cbx_3.Enabled = True
END IF
end event

type cbx_3 from checkbox within w_info_pallet_historico
integer x = 1330
integer y = 1600
integer width = 288
integer height = 68
integer taborder = 130
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_norden.Enabled = False
	em_norden.Text = ''
	ll_norden = -1
ELSE
	em_norden.Enabled = True
	ll_norden = Long(em_norden.Text)
END IF
end event

type em_norden from editmask within w_info_pallet_historico
integer x = 855
integer y = 1592
integer width = 361
integer height = 84
integer taborder = 180
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
string mask = "#####"
end type

event modified;ll_norden = Long(this.text)
end event

type st_21 from statictext within w_info_pallet_historico
integer x = 430
integer y = 1604
integer width = 402
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
string text = "Nro. Orden"
boolean focusrectangle = false
end type

type st_16 from statictext within w_info_pallet_historico
integer x = 251
integer y = 1552
integer width = 3072
integer height = 168
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_pallet_historico
integer x = 283
integer y = 996
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
string text = "Productor"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_pallet_historico
integer x = 2510
integer y = 1604
integer width = 411
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
string text = "Guía Control"
boolean focusrectangle = false
end type

type em_guiacontrol from editmask within w_info_pallet_historico
integer x = 2921
integer y = 1592
integer width = 361
integer height = 84
integer taborder = 190
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type st_17 from statictext within w_info_pallet_historico
integer x = 251
integer y = 1720
integer width = 3072
integer height = 136
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

type cbx_consolcalifi from checkbox within w_info_pallet_historico
integer x = 1577
integer y = 1744
integer width = 475
integer height = 76
integer taborder = 60
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
	
	cbx_todcalifi.Enabled = False
	ddlb_calificacion.SelectItem(0)
	cbx_todcalifi.Checked = True
	ddlb_calificacion.Enabled = False
	ii_calificacion = -9
	
ELSE	
	cbx_todcalifi.Enabled = True
	ii_calificacion = -1
END IF
end event

type cbx_todcalifi from checkbox within w_info_pallet_historico
integer x = 1280
integer y = 1744
integer width = 288
integer height = 76
integer taborder = 60
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN	
	
	ii_calificacion = -1
	ddlb_calificacion.SelectItem(0)
	ddlb_calificacion.Enabled = False
ELSE
	ddlb_calificacion.Enabled = True
	ii_calificacion = 0
	
END IF
end event

type ddlb_calificacion from dropdownlistbox within w_info_pallet_historico
integer x = 850
integer y = 1732
integer width = 389
integer height = 400
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string item[] = {"1","2","3","4","5"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_calificacion	=	(index)
end event

type st_29 from statictext within w_info_pallet_historico
integer x = 434
integer y = 1748
integer width = 366
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
string text = "Calificación"
boolean focusrectangle = false
end type

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_pallet_historico
integer x = 599
integer y = 820
integer taborder = 40
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

event ue_cambio;call super::ue_cambio;uo_selproductor.Filtra(-1,-1, uo_SelCliente.Codigo)
end event

type uo_selcliente from uo_seleccion_clientesprod within w_info_pallet_historico
integer x = 599
integer y = 460
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelProductor.Filtra(-1,-1, -1)
		
	Case Else
		uo_SelProductor.Filtra(-1,-1, This.Codigo)
		
End Choose
end event

type st_18 from statictext within w_info_pallet_historico
integer x = 279
integer y = 1464
integer width = 320
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
string text = "Recepcion"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde1 from editmask within w_info_pallet_historico
integer x = 626
integer y = 1448
integer width = 398
integer height = 96
integer taborder = 210
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

type em_hasta1 from editmask within w_info_pallet_historico
integer x = 1166
integer y = 1448
integer width = 402
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

type st_19 from statictext within w_info_pallet_historico
integer x = 1038
integer y = 1356
integer width = 105
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
string text = "-"
alignment alignment = center!
boolean focusrectangle = false
end type

