$PBExportHeader$w_info_control_calidad_recepcion_daños.srw
$PBExportComments$Ventana de Informe de Control Calidad de Daños y Defectos
forward
global type w_info_control_calidad_recepcion_daños from w_para_informes
end type
type st_3 from statictext within w_info_control_calidad_recepcion_daños
end type
type dw_especie from datawindow within w_info_control_calidad_recepcion_daños
end type
type st_10 from statictext within w_info_control_calidad_recepcion_daños
end type
type cbx_productor from checkbox within w_info_control_calidad_recepcion_daños
end type
type cbx_productorcons from checkbox within w_info_control_calidad_recepcion_daños
end type
type st_5 from statictext within w_info_control_calidad_recepcion_daños
end type
type st_16 from statictext within w_info_control_calidad_recepcion_daños
end type
type st_variedad from statictext within w_info_control_calidad_recepcion_daños
end type
type cbx_variedad from checkbox within w_info_control_calidad_recepcion_daños
end type
type cbx_planta from checkbox within w_info_control_calidad_recepcion_daños
end type
type st_6 from statictext within w_info_control_calidad_recepcion_daños
end type
type cbx_frigorifico from checkbox within w_info_control_calidad_recepcion_daños
end type
type cbx_especie from checkbox within w_info_control_calidad_recepcion_daños
end type
type st_origen from statictext within w_info_control_calidad_recepcion_daños
end type
type dw_tratamiento from datawindow within w_info_control_calidad_recepcion_daños
end type
type st_destino from statictext within w_info_control_calidad_recepcion_daños
end type
type dw_periodo from datawindow within w_info_control_calidad_recepcion_daños
end type
type cbx_tratamiento from checkbox within w_info_control_calidad_recepcion_daños
end type
type cbx_periodo from checkbox within w_info_control_calidad_recepcion_daños
end type
type cbx_tratcons from checkbox within w_info_control_calidad_recepcion_daños
end type
type cbx_periodocons from checkbox within w_info_control_calidad_recepcion_daños
end type
type st_planta from statictext within w_info_control_calidad_recepcion_daños
end type
type dw_frigorifico from uo_dddw_planta within w_info_control_calidad_recepcion_daños
end type
type st_8 from statictext within w_info_control_calidad_recepcion_daños
end type
type dw_camara from uo_dddw_planta within w_info_control_calidad_recepcion_daños
end type
type cbx_camara from checkbox within w_info_control_calidad_recepcion_daños
end type
type st_9 from statictext within w_info_control_calidad_recepcion_daños
end type
type cbx_grupo from checkbox within w_info_control_calidad_recepcion_daños
end type
type dw_grupo from datawindow within w_info_control_calidad_recepcion_daños
end type
type st_11 from statictext within w_info_control_calidad_recepcion_daños
end type
type cbx_subgrupo from checkbox within w_info_control_calidad_recepcion_daños
end type
type dw_subgrupo from datawindow within w_info_control_calidad_recepcion_daños
end type
type dw_variedad from datawindow within w_info_control_calidad_recepcion_daños
end type
type st_12 from statictext within w_info_control_calidad_recepcion_daños
end type
type cbx_grupocons from checkbox within w_info_control_calidad_recepcion_daños
end type
type cbx_sgrupocons from checkbox within w_info_control_calidad_recepcion_daños
end type
type dw_productor from datawindow within w_info_control_calidad_recepcion_daños
end type
type cbx_lote from checkbox within w_info_control_calidad_recepcion_daños
end type
type cbx_lotecons from checkbox within w_info_control_calidad_recepcion_daños
end type
type dw_planta from uo_dddw_planta within w_info_control_calidad_recepcion_daños
end type
type st_7 from statictext within w_info_control_calidad_recepcion_daños
end type
type st_4 from statictext within w_info_control_calidad_recepcion_daños
end type
type st_13 from statictext within w_info_control_calidad_recepcion_daños
end type
type dw_categoria from datawindow within w_info_control_calidad_recepcion_daños
end type
type cbx_categorias from checkbox within w_info_control_calidad_recepcion_daños
end type
type cbx_camaracons from checkbox within w_info_control_calidad_recepcion_daños
end type
type st_1 from statictext within w_info_control_calidad_recepcion_daños
end type
type dw_predio from datawindow within w_info_control_calidad_recepcion_daños
end type
type cbx_predio from checkbox within w_info_control_calidad_recepcion_daños
end type
type dw_ccosto from datawindow within w_info_control_calidad_recepcion_daños
end type
type st_2 from statictext within w_info_control_calidad_recepcion_daños
end type
type cbx_ccosto from checkbox within w_info_control_calidad_recepcion_daños
end type
type st_15 from statictext within w_info_control_calidad_recepcion_daños
end type
type st_17 from statictext within w_info_control_calidad_recepcion_daños
end type
type st_18 from statictext within w_info_control_calidad_recepcion_daños
end type
type em_desde from editmask within w_info_control_calidad_recepcion_daños
end type
type em_hasta from editmask within w_info_control_calidad_recepcion_daños
end type
type st_19 from statictext within w_info_control_calidad_recepcion_daños
end type
type dw_cliente from datawindow within w_info_control_calidad_recepcion_daños
end type
type st_14 from statictext within w_info_control_calidad_recepcion_daños
end type
type st_20 from statictext within w_info_control_calidad_recepcion_daños
end type
end forward

global type w_info_control_calidad_recepcion_daños from w_para_informes
integer x = 14
integer y = 32
integer width = 3598
integer height = 2244
string title = "Control de Calidad (Daños y Defectos)"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_3 st_3
dw_especie dw_especie
st_10 st_10
cbx_productor cbx_productor
cbx_productorcons cbx_productorcons
st_5 st_5
st_16 st_16
st_variedad st_variedad
cbx_variedad cbx_variedad
cbx_planta cbx_planta
st_6 st_6
cbx_frigorifico cbx_frigorifico
cbx_especie cbx_especie
st_origen st_origen
dw_tratamiento dw_tratamiento
st_destino st_destino
dw_periodo dw_periodo
cbx_tratamiento cbx_tratamiento
cbx_periodo cbx_periodo
cbx_tratcons cbx_tratcons
cbx_periodocons cbx_periodocons
st_planta st_planta
dw_frigorifico dw_frigorifico
st_8 st_8
dw_camara dw_camara
cbx_camara cbx_camara
st_9 st_9
cbx_grupo cbx_grupo
dw_grupo dw_grupo
st_11 st_11
cbx_subgrupo cbx_subgrupo
dw_subgrupo dw_subgrupo
dw_variedad dw_variedad
st_12 st_12
cbx_grupocons cbx_grupocons
cbx_sgrupocons cbx_sgrupocons
dw_productor dw_productor
cbx_lote cbx_lote
cbx_lotecons cbx_lotecons
dw_planta dw_planta
st_7 st_7
st_4 st_4
st_13 st_13
dw_categoria dw_categoria
cbx_categorias cbx_categorias
cbx_camaracons cbx_camaracons
st_1 st_1
dw_predio dw_predio
cbx_predio cbx_predio
dw_ccosto dw_ccosto
st_2 st_2
cbx_ccosto cbx_ccosto
st_15 st_15
st_17 st_17
st_18 st_18
em_desde em_desde
em_hasta em_hasta
st_19 st_19
dw_cliente dw_cliente
st_14 st_14
st_20 st_20
end type
global w_info_control_calidad_recepcion_daños w_info_control_calidad_recepcion_daños

type variables
uo_plantadesp			iuo_plantadesp
uo_camarasfrigo		iuo_camarasfrigo
uo_especie				iuo_especie
uo_grupoespecie		iuo_grupoespecie
uo_subgrupoespecie	iuo_subgrupoespecie
uo_variedades			iuo_variedades
uo_tratamientofrio	iuo_tratamientofrio
uo_periodofrio			iuo_periodofrio
uo_categorias			iuo_categorias
uo_productores			iuo_productores
uo_prodcuarteles			iuo_prodcuartel
uo_centrocostos		iuo_centrocostos

DataWindowChild	idwc_planta, idwc_frigorifico, idwc_categorias, idwc_camaras, idwc_especie, &
						idwc_grupo, idwc_subgrupo, idwc_variedad, idwc_tratamiento, idwc_periodo, &
						idwc_productor, idwc_predio, idwc_ccosto,idwc_exportadores

String	is_NomPlanta
Integer	ii_Planta, ii_Frigorifico, ii_Especie, ii_Grupo, ii_Predio, ii_ccosto, ii_cliente
Long     il_Productor
end variables

on w_info_control_calidad_recepcion_daños.create
int iCurrent
call super::create
this.st_3=create st_3
this.dw_especie=create dw_especie
this.st_10=create st_10
this.cbx_productor=create cbx_productor
this.cbx_productorcons=create cbx_productorcons
this.st_5=create st_5
this.st_16=create st_16
this.st_variedad=create st_variedad
this.cbx_variedad=create cbx_variedad
this.cbx_planta=create cbx_planta
this.st_6=create st_6
this.cbx_frigorifico=create cbx_frigorifico
this.cbx_especie=create cbx_especie
this.st_origen=create st_origen
this.dw_tratamiento=create dw_tratamiento
this.st_destino=create st_destino
this.dw_periodo=create dw_periodo
this.cbx_tratamiento=create cbx_tratamiento
this.cbx_periodo=create cbx_periodo
this.cbx_tratcons=create cbx_tratcons
this.cbx_periodocons=create cbx_periodocons
this.st_planta=create st_planta
this.dw_frigorifico=create dw_frigorifico
this.st_8=create st_8
this.dw_camara=create dw_camara
this.cbx_camara=create cbx_camara
this.st_9=create st_9
this.cbx_grupo=create cbx_grupo
this.dw_grupo=create dw_grupo
this.st_11=create st_11
this.cbx_subgrupo=create cbx_subgrupo
this.dw_subgrupo=create dw_subgrupo
this.dw_variedad=create dw_variedad
this.st_12=create st_12
this.cbx_grupocons=create cbx_grupocons
this.cbx_sgrupocons=create cbx_sgrupocons
this.dw_productor=create dw_productor
this.cbx_lote=create cbx_lote
this.cbx_lotecons=create cbx_lotecons
this.dw_planta=create dw_planta
this.st_7=create st_7
this.st_4=create st_4
this.st_13=create st_13
this.dw_categoria=create dw_categoria
this.cbx_categorias=create cbx_categorias
this.cbx_camaracons=create cbx_camaracons
this.st_1=create st_1
this.dw_predio=create dw_predio
this.cbx_predio=create cbx_predio
this.dw_ccosto=create dw_ccosto
this.st_2=create st_2
this.cbx_ccosto=create cbx_ccosto
this.st_15=create st_15
this.st_17=create st_17
this.st_18=create st_18
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.st_19=create st_19
this.dw_cliente=create dw_cliente
this.st_14=create st_14
this.st_20=create st_20
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.dw_especie
this.Control[iCurrent+3]=this.st_10
this.Control[iCurrent+4]=this.cbx_productor
this.Control[iCurrent+5]=this.cbx_productorcons
this.Control[iCurrent+6]=this.st_5
this.Control[iCurrent+7]=this.st_16
this.Control[iCurrent+8]=this.st_variedad
this.Control[iCurrent+9]=this.cbx_variedad
this.Control[iCurrent+10]=this.cbx_planta
this.Control[iCurrent+11]=this.st_6
this.Control[iCurrent+12]=this.cbx_frigorifico
this.Control[iCurrent+13]=this.cbx_especie
this.Control[iCurrent+14]=this.st_origen
this.Control[iCurrent+15]=this.dw_tratamiento
this.Control[iCurrent+16]=this.st_destino
this.Control[iCurrent+17]=this.dw_periodo
this.Control[iCurrent+18]=this.cbx_tratamiento
this.Control[iCurrent+19]=this.cbx_periodo
this.Control[iCurrent+20]=this.cbx_tratcons
this.Control[iCurrent+21]=this.cbx_periodocons
this.Control[iCurrent+22]=this.st_planta
this.Control[iCurrent+23]=this.dw_frigorifico
this.Control[iCurrent+24]=this.st_8
this.Control[iCurrent+25]=this.dw_camara
this.Control[iCurrent+26]=this.cbx_camara
this.Control[iCurrent+27]=this.st_9
this.Control[iCurrent+28]=this.cbx_grupo
this.Control[iCurrent+29]=this.dw_grupo
this.Control[iCurrent+30]=this.st_11
this.Control[iCurrent+31]=this.cbx_subgrupo
this.Control[iCurrent+32]=this.dw_subgrupo
this.Control[iCurrent+33]=this.dw_variedad
this.Control[iCurrent+34]=this.st_12
this.Control[iCurrent+35]=this.cbx_grupocons
this.Control[iCurrent+36]=this.cbx_sgrupocons
this.Control[iCurrent+37]=this.dw_productor
this.Control[iCurrent+38]=this.cbx_lote
this.Control[iCurrent+39]=this.cbx_lotecons
this.Control[iCurrent+40]=this.dw_planta
this.Control[iCurrent+41]=this.st_7
this.Control[iCurrent+42]=this.st_4
this.Control[iCurrent+43]=this.st_13
this.Control[iCurrent+44]=this.dw_categoria
this.Control[iCurrent+45]=this.cbx_categorias
this.Control[iCurrent+46]=this.cbx_camaracons
this.Control[iCurrent+47]=this.st_1
this.Control[iCurrent+48]=this.dw_predio
this.Control[iCurrent+49]=this.cbx_predio
this.Control[iCurrent+50]=this.dw_ccosto
this.Control[iCurrent+51]=this.st_2
this.Control[iCurrent+52]=this.cbx_ccosto
this.Control[iCurrent+53]=this.st_15
this.Control[iCurrent+54]=this.st_17
this.Control[iCurrent+55]=this.st_18
this.Control[iCurrent+56]=this.em_desde
this.Control[iCurrent+57]=this.em_hasta
this.Control[iCurrent+58]=this.st_19
this.Control[iCurrent+59]=this.dw_cliente
this.Control[iCurrent+60]=this.st_14
this.Control[iCurrent+61]=this.st_20
end on

on w_info_control_calidad_recepcion_daños.destroy
call super::destroy
destroy(this.st_3)
destroy(this.dw_especie)
destroy(this.st_10)
destroy(this.cbx_productor)
destroy(this.cbx_productorcons)
destroy(this.st_5)
destroy(this.st_16)
destroy(this.st_variedad)
destroy(this.cbx_variedad)
destroy(this.cbx_planta)
destroy(this.st_6)
destroy(this.cbx_frigorifico)
destroy(this.cbx_especie)
destroy(this.st_origen)
destroy(this.dw_tratamiento)
destroy(this.st_destino)
destroy(this.dw_periodo)
destroy(this.cbx_tratamiento)
destroy(this.cbx_periodo)
destroy(this.cbx_tratcons)
destroy(this.cbx_periodocons)
destroy(this.st_planta)
destroy(this.dw_frigorifico)
destroy(this.st_8)
destroy(this.dw_camara)
destroy(this.cbx_camara)
destroy(this.st_9)
destroy(this.cbx_grupo)
destroy(this.dw_grupo)
destroy(this.st_11)
destroy(this.cbx_subgrupo)
destroy(this.dw_subgrupo)
destroy(this.dw_variedad)
destroy(this.st_12)
destroy(this.cbx_grupocons)
destroy(this.cbx_sgrupocons)
destroy(this.dw_productor)
destroy(this.cbx_lote)
destroy(this.cbx_lotecons)
destroy(this.dw_planta)
destroy(this.st_7)
destroy(this.st_4)
destroy(this.st_13)
destroy(this.dw_categoria)
destroy(this.cbx_categorias)
destroy(this.cbx_camaracons)
destroy(this.st_1)
destroy(this.dw_predio)
destroy(this.cbx_predio)
destroy(this.dw_ccosto)
destroy(this.st_2)
destroy(this.cbx_ccosto)
destroy(this.st_15)
destroy(this.st_17)
destroy(this.st_18)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.st_19)
destroy(this.dw_cliente)
destroy(this.st_14)
destroy(this.st_20)
end on

event open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp
iuo_camarasfrigo		=	Create uo_camarasfrigo
iuo_especie				=	Create uo_especie
iuo_grupoespecie		=	Create uo_grupoespecie
iuo_subgrupoespecie	=	Create uo_subgrupoespecie
iuo_variedades			=	Create uo_variedades
iuo_tratamientofrio	=	Create uo_tratamientofrio
iuo_periodofrio		=	Create uo_periodofrio
iuo_categorias			=	Create uo_categorias
iuo_productores		=	Create uo_productores
iuo_prodcuartel		=	Create uo_prodcuarteles
iuo_centrocostos		=	Create uo_centrocostos

em_desde.SetFocus()


//exportador

dw_cliente.GetChild("clie_codigo", idwc_exportadores)
idwc_exportadores.SetTransObject(sqlca)
IF idwc_exportadores.Retrieve() = 0 THEN
//	MessageBox("Atención","Falta Registrar Plantas")
ELSE
	dw_cliente.SetTransObject(sqlca)
	dw_cliente.InsertRow(0)
END IF

//Planta
dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
//	MessageBox("Atención","Falta Registrar Plantas")
ELSE
	dw_planta.SetTransObject(sqlca)
	dw_planta.InsertRow(0)
END IF

//Frigorifico
dw_frigorifico.GetChild("plde_codigo", idwc_frigorifico)
idwc_frigorifico.SetTransObject(sqlca)
IF idwc_frigorifico.Retrieve() = 0 THEN
	//MessageBox("Atención","Falta Registrar Frigorificos")
ELSE
	dw_frigorifico.SetTransObject(sqlca)
	dw_frigorifico.InsertRow(0)
END IF

//Camara Frigorifico
dw_camara.GetChild("cama_codigo", idwc_camaras)
idwc_camaras.SetTransObject(sqlca)
IF idwc_camaras.Retrieve(99) = 0 THEN
	//MessageBox("Atención","Falta Registrar Camaras Frigorificos")
	dw_camara.InsertRow(0)
ELSE
	dw_camara.SetTransObject(sqlca)
	dw_camara.InsertRow(0)
END IF

//Especie
dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	//MessageBox("Atención","Falta Registrar Especies")
ELSE
	dw_especie.SetTransObject(sqlca)
	dw_especie.InsertRow(0)
END IF

//Grupo
dw_grupo.GetChild("grva_codigo",idwc_grupo)
idwc_grupo.SetTransObject(Sqlca)
IF idwc_grupo.Retrieve(0) = 0 THEN
	//MessageBox("Atención","Falta Registrar Grupos")
ELSE
	dw_grupo.SetTransObject(Sqlca)
	dw_grupo.InsertRow(0)
END IF

//Sub Grupo
dw_subgrupo.GetChild("grva_codsub",idwc_subgrupo)
idwc_subgrupo.SetTransObject(Sqlca)
IF idwc_subgrupo.Retrieve(0,0) = 0 THEN
	//MessageBox("Atención","Falta Registrar Sub Grupos")
ELSE
	dw_subgrupo.SetTransObject(Sqlca)
	dw_subgrupo.InsertRow(0)
END IF

//Variedad
dw_variedad.GetChild("vari_codigo",idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
idwc_variedad.InsertRow(0)


//Tratamiento
dw_tratamiento.GetChild("frio_tipofr",idwc_tratamiento)
idwc_tratamiento.SetTransObject(Sqlca)
IF idwc_tratamiento.Retrieve() = 0 THEN
	//MessageBox("Atención","Falta Registrar Tratamientos")
ELSE
	dw_tratamiento.SetTransObject(Sqlca)
	dw_tratamiento.InsertRow(0)
END IF

//Periodo
dw_periodo.GetChild("frio_tipofr",idwc_periodo)
idwc_periodo.SetTransObject(Sqlca)
IF idwc_periodo.Retrieve() = 0 THEN
	//MessageBox("Atención","Falta Registrar Periodos")
ELSE
	dw_periodo.SetTransObject(Sqlca)
	dw_periodo.InsertRow(0)
END IF

//Categorias
dw_categoria.GetChild("cate_codigo",idwc_categorias)
idwc_categorias.SetTransObject(Sqlca)
IF idwc_periodo.Retrieve() = 0 THEN
	//MessageBox("Atención","Falta Registrar Categorias")
ELSE
	dw_categoria.SetTransObject(Sqlca)
	dw_categoria.InsertRow(0)
END IF

//Productor
dw_productor.GetChild("prod_codigo",idwc_productor)
idwc_productor.SetTransObject(Sqlca)
IF idwc_productor.Retrieve(gi_codexport) = 0 THEN
	//MessageBox("Atención","Falta Registrar Productores")
ELSE
	dw_productor.SetTransObject(Sqlca)
	dw_productor.InsertRow(0)
END IF

//Predio
dw_predio.GetChild("prbr_codpre", idwc_predio)
idwc_predio.SetTransObject(sqlca)
IF idwc_predio.Retrieve(0) = 0 THEN
	//MessageBox("Atención","Falta Registrar Predios")
	dw_predio.InsertRow(0)
ELSE
	dw_predio.SetTransObject(sqlca)
	dw_predio.InsertRow(0)
END IF

//Centro Costo
dw_ccosto.GetChild("prcc_codigo", idwc_planta)
idwc_ccosto.SetTransObject(sqlca)
IF idwc_ccosto.Retrieve(0,0) = 0 THEN
	//MessageBox("Atención","Falta Registrar Centrode Costo")
	dw_ccosto.InsertRow(0)
ELSE
	dw_ccosto.SetTransObject(sqlca)
	dw_ccosto.InsertRow(0)
END IF

ii_cliente = GI_CodExport

dw_planta.Object.plde_codigo.BackGround.Color		=	RGB(192, 192, 192)
dw_frigorifico.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)
dw_camara.Object.cama_codigo.BackGround.Color		=	RGB(192, 192, 192)
dw_especie.Object.espe_codigo.BackGround.Color		=	RGB(192, 192, 192)
dw_grupo.Object.grva_codigo.BackGround.Color			=	RGB(192, 192, 192)
dw_subgrupo.Object.grva_codsub.BackGround.Color		=	RGB(192, 192, 192)
dw_variedad.Object.vari_codigo.BackGround.Color		=	RGB(192, 192, 192)
dw_tratamiento.Object.frio_tipofr.BackGround.Color	=	RGB(192, 192, 192)
dw_periodo.Object.pefr_codigo.BackGround.Color		=	RGB(192, 192, 192)
dw_categoria.Object.cate_codigo.BackGround.Color	=	RGB(192, 192, 192)
dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(192, 192, 192)
dw_predio.Object.prbr_codpre.BackGround.Color		=	RGB(192, 192, 192)
end event

type pb_excel from w_para_informes`pb_excel within w_info_control_calidad_recepcion_daños
end type

type st_computador from w_para_informes`st_computador within w_info_control_calidad_recepcion_daños
end type

type st_usuario from w_para_informes`st_usuario within w_info_control_calidad_recepcion_daños
end type

type st_temporada from w_para_informes`st_temporada within w_info_control_calidad_recepcion_daños
end type

type p_logo from w_para_informes`p_logo within w_info_control_calidad_recepcion_daños
end type

type st_titulo from w_para_informes`st_titulo within w_info_control_calidad_recepcion_daños
integer x = 78
integer y = 64
integer width = 3136
string text = "Informe Control de Calidad de Recepción (Daños y Defectos)"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_control_calidad_recepcion_daños
integer x = 3310
integer y = 628
integer height = 140
integer taborder = 180
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	li_Planta, li_Frigorifico, li_Camara, li_Especie, li_Grupo, li_SubGrupo, &
			li_Variedad, li_Periodo, li_Categoria, li_NroLote, &
			li_ConsCamara, li_ConsGrupo, li_ConsSubGrupo, li_ConsTrat, li_ConsPeriodo, &
			li_ConsProductor, li_ConsLote, li_CCosto, li_Predio
Long		ll_Fila, ll_Productor
String	ls_Tratamiento
Date		ld_Desde, ld_Hasta

istr_info.titulo	= 'EXISTENCIA DE FRUTA GRANEL'

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_recepcion_daños_y_defectos"

IF IsNull(em_Desde.Text) OR IsNull(em_Hasta.Text) THEN
	MessageBox("Aencion","Debe ingresar las fechas de Recepcion")
ELSE
	
	
	IF cbx_planta.Checked THEN
		li_Planta = 0
	ELSE
		li_Planta = dw_planta.Object.plde_codigo[1]
	END IF
	
	IF cbx_frigorifico.Checked THEN
		li_Frigorifico = 0
	ELSE
		li_Frigorifico = dw_frigorifico.Object.plde_codigo[1]
	END IF
	
	IF cbx_camara.Checked THEN
		li_Camara = 0
		IF cbx_camaracons.Checked THEN	
			li_ConsCamara	=	1
		ELSE
			li_ConsCamara	=	0
		END IF
	ELSE
		li_Camara	=	dw_camara.Object.cama_codigo[1]
	END IF
	
	IF cbx_especie.Checked THEN
		li_Especie = 0
	ELSE
		li_Especie = dw_especie.Object.espe_codigo[1]
	END IF
	
	IF cbx_grupo.Checked THEN
		li_Grupo = 0
		IF cbx_grupocons.Checked THEN	
			li_ConsGrupo =	1
		ELSE
			li_ConsGrupo = 0
		END IF
	ELSE
		li_Grupo = dw_grupo.Object.grva_codigo[1]
	END IF
	
	IF cbx_subgrupo.Checked THEN
		li_SubGrupo = 0
		IF cbx_sgrupocons.Checked THEN	
			li_ConsGrupo =	1
		ELSE
			li_ConsGrupo = 0
		END IF
	ELSE
		li_SubGrupo = dw_subgrupo.Object.grva_codsub[1]
	END IF
	
	IF cbx_variedad.Checked THEN
		li_Variedad = 0
	ELSE
		li_Variedad = dw_variedad.Object.vari_codigo[1]
	END IF
	
	IF cbx_tratamiento.Checked THEN
		ls_Tratamiento = 'Z'
		IF cbx_tratamiento.Checked THEN	
			li_ConsTrat =	1
		ELSE
			li_ConsTrat = 0
		END IF
	ELSE
		ls_Tratamiento = dw_tratamiento.Object.frio_tipofr[1]
	END IF
	
	IF cbx_periodo.Checked THEN
		li_Periodo = 0
		IF cbx_periodocons.Checked THEN	
			li_ConsPeriodo =	1
		ELSE
			li_ConsPeriodo = 0
		END IF
	ELSE
		li_Periodo = dw_periodo.Object.pefr_codigo[1]
	END IF
	
	IF cbx_categorias.Checked THEN
		li_Categoria = 0
	ELSE
		li_Categoria = dw_categoria.Object.cate_codigo[1]
	END IF
	
	IF cbx_productor.Checked THEN
		ll_Productor = 0
		IF cbx_productorcons.Checked THEN	
			li_ConsProductor =	1
		ELSE
			li_ConsProductor = 0
		END IF
	ELSE
		ll_Productor = dw_productor.Object.prod_codigo[1]
	END IF
	
	IF cbx_lote.Checked THEN
		li_Nrolote = 0
		IF cbx_lotecons.Checked THEN	
			li_ConsLote =	1
		ELSE
			li_ConsLote = 0
		END IF
	END IF
	
	IF cbx_predio.Checked THEN
		li_Predio = 0
	ELSE
		li_Predio = dw_predio.Object.prbr_codpre[1]
	END IF
	
	IF cbx_ccosto.Checked THEN
		li_CCosto = 0
	ELSE
		li_CCosto = dw_ccosto.Object.prcc_codigo[1]
	END IF
	
	vinf.dw_1.SetTransObject(sqlca)
	
	ll_Fila	=	vinf.dw_1.Retrieve(li_Planta,li_Frigorifico,li_Camara,li_Especie,li_Grupo,&
					li_SubGrupo,li_Variedad,ls_Tratamiento,li_Periodo,li_Categoria,&
					ll_Productor,li_NroLote,li_ConsCamara,li_ConsGrupo,li_ConsSubGrupo,&
					li_ConsTrat,li_ConsPeriodo,li_ConsProductor,li_ConsLote, li_Ccosto,&
					ld_Desde,ld_Hasta)
		
	IF ll_Fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
		
	ELSEIF ll_Fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
						 StopSign!, Ok!)
		
	ELSE
		F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
		vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')
		vinf.Visible	= True
		vinf.Enabled	= True
	END IF
END IF
SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_control_calidad_recepcion_daños
integer x = 3310
integer y = 916
integer height = 140
integer taborder = 190
end type

type st_3 from statictext within w_info_control_calidad_recepcion_daños
integer x = 169
integer y = 1308
integer width = 315
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_info_control_calidad_recepcion_daños
integer x = 672
integer y = 1296
integer width = 887
integer height = 92
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

event itemchanged;Integer	li_Null,li_cliente

SetNull(li_Null)
dw_cliente.accepttext()

li_cliente  = dw_cliente.Object.clie_codigo[1]

ii_Especie	=	Integer(data)

IF NOT iuo_Especie.Existe(Integer(data),True,SqlCa) THEN
	This.SetItem(1, "espe_codigo", li_Null )
	This.SetFocus()
	RETURN 1
ELSE
	dw_grupo.GetChild("grva_codigo",idwc_grupo)
	idwc_grupo.SetTransObject(Sqlca)
	dw_grupo.Reset()
	dw_grupo.InsertRow(0)
	idwc_grupo.Retrieve(ii_Especie)
	/**/
	dw_subgrupo.GetChild("grva_codsub",idwc_subgrupo)
	idwc_subgrupo.SetTransObject(Sqlca)
	dw_subgrupo.Reset()
	dw_subgrupo.InsertRow(0)
	idwc_subgrupo.Retrieve(ii_Especie,0)
	/**/
	dw_variedad.GetChild("vari_codigo",idwc_variedad)
	idwc_variedad.SetTransObject(Sqlca)
	dw_variedad.Reset()
	dw_variedad.InsertRow(0)
	idwc_variedad.Retrieve(ii_Especie)
END IF

end event

type st_10 from statictext within w_info_control_calidad_recepcion_daños
integer x = 1783
integer y = 1308
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Productor"
boolean focusrectangle = false
end type

type cbx_productor from checkbox within w_info_control_calidad_recepcion_daños
integer x = 2245
integer y = 1216
integer width = 283
integer height = 80
integer taborder = 160
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;String	ls_Null

SetNull(ls_Null)

IF This.Checked THEN
	dw_productor.SetItem(1,"prod_codigo",Long(ls_Null))
	cbx_productorcons.Enabled	=	True
	dw_productor.Enabled			=	False
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(192, 192, 192)
ELSE
	cbx_productorcons.Enabled	=	False
	cbx_productorcons.Checked	=	False
	dw_productor.Enabled			=	True
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_productor.SetFocus()
END IF

end event

type cbx_productorcons from checkbox within w_info_control_calidad_recepcion_daños
integer x = 2665
integer y = 1220
integer width = 443
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Consolidado"
end type

type st_5 from statictext within w_info_control_calidad_recepcion_daños
integer x = 78
integer y = 1188
integer width = 1600
integer height = 840
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_16 from statictext within w_info_control_calidad_recepcion_daños
integer x = 1682
integer y = 1188
integer width = 1531
integer height = 840
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_control_calidad_recepcion_daños
integer x = 169
integer y = 1880
integer width = 315
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Variedad"
boolean focusrectangle = false
end type

type cbx_variedad from checkbox within w_info_control_calidad_recepcion_daños
integer x = 677
integer y = 1788
integer width = 283
integer height = 80
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Todas"
boolean checked = true
end type

event clicked;String	ls_Null

SetNull(ls_Null)

IF This.Checked THEN
	dw_variedad.SetItem(1,"vari_codigo",Integer(ls_Null))
	dw_variedad.Enabled			=	False
	dw_variedad.Object.vari_codigo.BackGround.Color	=	RGB(192, 192, 192)
ELSE
	dw_variedad.Enabled			=	True
	dw_variedad.Object.vari_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_variedad.SetFocus()
END IF
end event

type cbx_planta from checkbox within w_info_control_calidad_recepcion_daños
integer x = 672
integer y = 532
integer width = 283
integer height = 76
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todas"
boolean checked = true
end type

event clicked;String	ls_Null

SetNull(ls_Null)

IF This.Checked THEN
	dw_planta.SetItem(1,"plde_codigo",Integer(ls_Null))
	dw_planta.Enabled		=	False
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)
ELSE
	dw_planta.Enabled		=	True
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_planta.SetFocus()
END IF
end event

type st_6 from statictext within w_info_control_calidad_recepcion_daños
integer x = 169
integer y = 828
integer width = 315
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Frigorífico"
boolean focusrectangle = false
end type

type cbx_frigorifico from checkbox within w_info_control_calidad_recepcion_daños
integer x = 672
integer y = 728
integer width = 283
integer height = 76
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;String	ls_Null

SetNull(ls_Null)

IF This.Checked THEN
	dw_frigorifico.SetItem(1,"plde_codigo",Integer(ls_Null))
	dw_frigorifico.Enabled		=	False
	dw_frigorifico.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)
ELSE
	dw_frigorifico.Enabled		=	True
	dw_frigorifico.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_frigorifico.SetFocus()
END IF
end event

type cbx_especie from checkbox within w_info_control_calidad_recepcion_daños
integer x = 672
integer y = 1216
integer width = 283
integer height = 80
integer taborder = 70
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
boolean checked = true
end type

event clicked;Integer	li_Null

SetNull(li_Null)

IF This.Checked THEN
	dw_especie.SetItem(1,"espe_codigo",li_Null)
	dw_especie.Enabled			=	False
	dw_especie.Object.espe_codigo.BackGround.Color	=	RGB(192, 192, 192)
	dw_grupo.SetItem(1,"grva_codigo",li_Null)
	dw_grupo.Enabled				=	False
	dw_grupo.Object.grva_codigo.BackGround.Color		=	RGB(192, 192, 192)
	dw_subgrupo.SetItem(1,"grva_codsub",li_Null)
	dw_subgrupo.Enabled			=	False
	dw_subgrupo.Object.grva_codsub.BackGround.Color	=	RGB(192, 192, 192)
	dw_variedad.SetItem(1,"vari_codigo",li_Null)
	dw_variedad.Enabled			=	False
	dw_variedad.Object.vari_codigo.BackGround.Color	=	RGB(192, 192, 192)
	cbx_variedad.Checked			=	True
	cbx_variedad.Enabled			=	False
	cbx_grupo.Checked				=	True
	cbx_grupo.Enabled				=	False
	cbx_subgrupo.Checked			=	True
	cbx_subgrupo.Enabled			=	False
//	cbx_grupocons.Enabled		=	False
//	cbx_sgrupocons.Enabled		=	False
ELSE
	dw_especie.Enabled			=	True
	dw_especie.Object.espe_codigo.BackGround.Color	=	RGB(255, 255, 255)
	IF NOT cbx_grupo.Checked THEN
		dw_grupo.Enabled			=	True
		dw_grupo.Object.grva_codigo.BackGround.Color		=	RGB(255, 255, 255)
	END IF
	IF NOT cbx_subgrupo.Checked THEN
		dw_subgrupo.Enabled			=	True
		dw_subgrupo.Object.grva_codsub.BackGround.Color	=	RGB(255, 255, 255)
	END IF
	IF NOT cbx_variedad.Checked THEN
		dw_variedad.Enabled			=	True
		dw_variedad.Object.vari_codigo.BackGround.Color	=	RGB(255, 255, 255)
	END IF
	dw_especie.SetFocus()
	cbx_variedad.Enabled			=	True
	cbx_grupo.Enabled				=	True
	cbx_subgrupo.Enabled			=	True
//	cbx_grupocons.Enabled		=	True
//	cbx_sgrupocons.Enabled		=	True
END IF
end event

type st_origen from statictext within w_info_control_calidad_recepcion_daños
integer x = 1783
integer y = 612
integer width = 439
integer height = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Tratamiento"
boolean focusrectangle = false
end type

type dw_tratamiento from datawindow within w_info_control_calidad_recepcion_daños
integer x = 2245
integer y = 612
integer width = 873
integer height = 92
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_tratamientofrio"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null

SetNull(li_Null)

IF NOT iuo_TratamientoFrio.Ofp_Recupera_TratamientoFrio(SqlCa,data,True) THEN
	This.SetItem(1, "frio_tipofr", String(li_Null))
	This.SetFocus()
	RETURN 1
END IF

end event

event itemerror;RETURN 1
end event

type st_destino from statictext within w_info_control_calidad_recepcion_daños
integer x = 1783
integer y = 820
integer width = 439
integer height = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Periodo"
boolean focusrectangle = false
end type

type dw_periodo from datawindow within w_info_control_calidad_recepcion_daños
integer x = 2245
integer y = 812
integer width = 882
integer height = 100
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_periodofrio"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null

SetNull(li_Null)

IF NOT iuo_PeriodoFrio.Ofp_Recupera_PeriodoFrio(SqlCa,Integer(data),True) THEN
	This.SetItem(1, "pefr_codigo", li_Null)
	This.SetFocus()
	RETURN 1
END IF

end event

event itemerror;RETURN 1
end event

type cbx_tratamiento from checkbox within w_info_control_calidad_recepcion_daños
integer x = 2245
integer y = 532
integer width = 283
integer height = 76
integer taborder = 120
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;String	ls_Null

SetNull(ls_Null)

IF This.Checked THEN
	dw_tratamiento.SetItem(1,"frio_tipofr",ls_Null)
	cbx_tratcons.Enabled			=	True
	dw_tratamiento.Enabled		=	False
	dw_tratamiento.Object.frio_tipofr.BackGround.Color	=	RGB(192, 192, 192)
ELSE
	cbx_tratcons.Enabled			=	False
	cbx_tratcons.Checked			=	False
	dw_tratamiento.Enabled		=	True
	dw_tratamiento.Object.frio_tipofr.BackGround.Color	=	RGB(255, 255, 255)
	dw_tratamiento.SetFocus()
END IF
end event

type cbx_periodo from checkbox within w_info_control_calidad_recepcion_daños
integer x = 2245
integer y = 728
integer width = 283
integer height = 76
integer taborder = 140
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;String	ls_Null

SetNull(ls_Null)

IF This.Checked THEN
	dw_periodo.SetItem(1,"pefr_codigo",Integer(ls_Null))
	cbx_periodocons.Enabled	=	True
	dw_periodo.Enabled		=	False
	dw_periodo.Object.pefr_codigo.BackGround.Color	=	RGB(192, 192, 192)
ELSE
	cbx_periodocons.Enabled	=	False
	cbx_periodocons.Checked	=	False
	dw_periodo.Enabled		=	True
	dw_periodo.Object.pefr_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_periodo.SetFocus()
END IF
end event

type cbx_tratcons from checkbox within w_info_control_calidad_recepcion_daños
integer x = 2665
integer y = 532
integer width = 443
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Consolidado"
end type

type cbx_periodocons from checkbox within w_info_control_calidad_recepcion_daños
integer x = 2665
integer y = 728
integer width = 443
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Consolidado"
end type

type st_planta from statictext within w_info_control_calidad_recepcion_daños
integer x = 169
integer y = 620
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Planta"
boolean focusrectangle = false
end type

type dw_frigorifico from uo_dddw_planta within w_info_control_calidad_recepcion_daños
integer x = 667
integer y = 816
integer width = 887
boolean bringtotop = true
integer ii_tipoplanta = 2
end type

event itemchanged;call super::itemchanged;Integer	li_Null

SetNull(li_Null)

ii_Frigorifico	= Integer(data)

IF NOT iuo_PlantaDesp.Existe(Integer(data),True,SqlCa) THEN
	This.SetItem(1, "plde_codigo", li_Null )
	This.SetFocus()
	RETURN 1
ELSE
	dw_camara.GetChild("cama_codigo", idwc_camaras)
	idwc_camaras.SetTransObject(sqlca)
	dw_camara.Reset()
	dw_camara.InsertRow(0)
	idwc_camaras.Retrieve(ii_Frigorifico)
END IF

end event

event itemerror;call super::itemerror;RETURN 1
end event

type st_8 from statictext within w_info_control_calidad_recepcion_daños
integer x = 169
integer y = 1064
integer width = 325
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Camara"
boolean focusrectangle = false
end type

type dw_camara from uo_dddw_planta within w_info_control_calidad_recepcion_daños
integer x = 667
integer y = 1052
integer width = 887
boolean bringtotop = true
string dataobject = "dddw_camarasfrigo"
end type

event itemchanged;call super::itemchanged;Integer	li_Null

SetNull(li_Null)

IF NOT iuo_CamarasFrigo.Existe(Integer(ii_Frigorifico),Integer(data),True,SqlCa) THEN
	This.SetItem(1, "cama_codigo", li_Null )
	This.SetFocus()
	RETURN 1
END IF

end event

event itemerror;call super::itemerror;RETURN 1
end event

type cbx_camara from checkbox within w_info_control_calidad_recepcion_daños
integer x = 672
integer y = 968
integer width = 283
integer height = 76
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;String	ls_Null

SetNull(ls_Null)

IF This.Checked THEN
	dw_camara.SetItem(1,"cama_codigo",ls_Null)
	cbx_camaracons.Enabled			=	True
	dw_camara.Enabled		=	False
	dw_camara.Object.cama_codigo.BackGround.Color	=	RGB(192, 192, 192)
ELSE
	cbx_camaracons.Enabled			=	False
	cbx_camaracons.Checked			=	False
	dw_camara.Enabled		=	True
	dw_camara.Object.cama_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_camara.SetFocus()
END IF
end event

type st_9 from statictext within w_info_control_calidad_recepcion_daños
integer x = 169
integer y = 1504
integer width = 434
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Grupo Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_grupo from checkbox within w_info_control_calidad_recepcion_daños
integer x = 672
integer y = 1412
integer width = 283
integer height = 80
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Todas"
boolean checked = true
end type

event clicked;String	ls_Null

SetNull(ls_Null)

IF This.Checked THEN
	dw_grupo.SetItem(1,"grva_codigo",Integer(ls_Null))
	cbx_grupocons.Enabled	=	True
	dw_grupo.Enabled			=	False
	dw_grupo.Object.grva_codigo.BackGround.Color	=	RGB(192, 192, 192)
ELSE
//	cbx_grupocons.Enabled	=	False
//	cbx_grupocons.Checked	=	False
	dw_grupo.Enabled			=	True
	dw_grupo.Object.grva_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_grupo.SetFocus()
END IF
end event

type dw_grupo from datawindow within w_info_control_calidad_recepcion_daños
integer x = 672
integer y = 1492
integer width = 887
integer height = 92
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_grupos"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Null

SetNull(li_Null)

ii_Grupo	=	Integer(data)

IF NOT iuo_GrupoEspecie.Existe(ii_Especie,Integer(data),True,SqlCa) THEN
	This.SetItem(1, "grva_codigo", li_Null )
	This.SetFocus()
	RETURN 1
ELSE
	dw_subgrupo.GetChild("grva_codsub",idwc_subgrupo)
	idwc_subgrupo.SetTransObject(Sqlca)
	dw_subgrupo.Reset()
	dw_subgrupo.InsertRow(0)
	idwc_subgrupo.Retrieve(ii_Especie,ii_Grupo)
END IF
end event

event itemerror;RETURN 1
end event

type st_11 from statictext within w_info_control_calidad_recepcion_daños
integer x = 169
integer y = 1692
integer width = 466
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Sub Grupo Esp."
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_subgrupo from checkbox within w_info_control_calidad_recepcion_daños
integer x = 672
integer y = 1600
integer width = 283
integer height = 80
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Todas"
boolean checked = true
end type

event clicked;String	ls_Null

SetNull(ls_Null)

IF This.Checked THEN
	dw_subgrupo.SetItem(1,"grva_codsub",Integer(ls_Null))
	cbx_sgrupocons.Enabled		=	True
	dw_subgrupo.Enabled			=	False
	dw_subgrupo.Object.grva_codsub.BackGround.Color	=	RGB(192, 192, 192)
ELSE
//	cbx_sgrupocons.Enabled		=	False
//	cbx_sgrupocons.Checked		=	False
	dw_subgrupo.Enabled			=	True
	dw_subgrupo.Object.grva_codsub.BackGround.Color	=	RGB(255, 255, 255)
	dw_subgrupo.SetFocus()
END IF
end event

type dw_subgrupo from datawindow within w_info_control_calidad_recepcion_daños
integer x = 672
integer y = 1680
integer width = 887
integer height = 92
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_subgrupos"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Null

SetNull(li_Null)

IF NOT iuo_SubGrupoEspecie.Existe(ii_Especie,ii_Grupo,Integer(data),True,SqlCa) THEN
	This.SetItem(1, "grva_codsub", li_Null )
	This.SetFocus()
	RETURN 1
END IF

end event

event itemerror;RETURN 1
end event

type dw_variedad from datawindow within w_info_control_calidad_recepcion_daños
integer x = 672
integer y = 1868
integer width = 887
integer height = 92
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_variedades"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

event itemchanged;Integer	li_Null,li_cliente

SetNull(li_Null)

dw_cliente.accepttext()

li_cliente  = dw_cliente.Object.clie_codigo[1]


IF NOT iuo_Variedades.Existe(ii_Especie,Integer(data),True,SqlCa) THEN
	This.SetItem(1, "vari_codigo", li_Null )
	This.SetFocus()
	RETURN 1
END IF
end event

type st_12 from statictext within w_info_control_calidad_recepcion_daños
integer x = 1783
integer y = 1880
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Nro. Lote"
boolean focusrectangle = false
end type

type cbx_grupocons from checkbox within w_info_control_calidad_recepcion_daños
integer x = 1106
integer y = 1416
integer width = 443
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Consolidado"
end type

type cbx_sgrupocons from checkbox within w_info_control_calidad_recepcion_daños
integer x = 1106
integer y = 1604
integer width = 443
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Consolidado"
end type

type dw_productor from datawindow within w_info_control_calidad_recepcion_daños
integer x = 2245
integer y = 1296
integer width = 887
integer height = 92
boolean bringtotop = true
string dataobject = "dddw_productores"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Null,li_cliente

SetNull(li_Null)

dw_cliente.accepttext()

li_cliente  = dw_cliente.Object.clie_codigo[1]


il_Productor	=	Long(Data)
IF NOT iuo_Productores.Existe(Long(data),True,SqlCa) THEN
	This.SetItem(1, "prod_codigo", li_Null)
	This.SetFocus()
	RETURN 1
ELSE
	dw_predio.GetChild("prpr_codpre",idwc_predio)
	idwc_predio.SetTransObject(Sqlca)
	dw_predio.Reset()
	dw_predio.InsertRow(0)
	idwc_predio.Retrieve(il_Productor)
END IF

end event

event itemerror;RETURN 1
end event

type cbx_lote from checkbox within w_info_control_calidad_recepcion_daños
integer x = 2245
integer y = 1876
integer width = 283
integer height = 72
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_lotecons.Enabled		=	True
ELSE
	cbx_lotecons.Enabled		=	False
	cbx_lotecons.Checked		=	False
END IF
end event

type cbx_lotecons from checkbox within w_info_control_calidad_recepcion_daños
integer x = 2674
integer y = 1876
integer width = 443
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Consolidado"
end type

type dw_planta from uo_dddw_planta within w_info_control_calidad_recepcion_daños
integer x = 672
integer y = 612
boolean bringtotop = true
integer ii_tipoplanta = 1
end type

event itemchanged;call super::itemchanged;Integer	li_Null

SetNull(li_Null)

IF NOT iuo_PlantaDesp.existe(Integer(data),True,SqlCa) THEN
	This.SetItem(1, "plde_codigo", li_Null )
	This.SetFocus()
	RETURN 1
ELSE
	ii_Planta	=	Integer(data)
END IF

end event

event itemerror;call super::itemerror;RETURN 1
end event

type st_7 from statictext within w_info_control_calidad_recepcion_daños
integer x = 1682
integer y = 504
integer width = 1531
integer height = 684
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_control_calidad_recepcion_daños
integer x = 78
integer y = 504
integer width = 1600
integer height = 684
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_control_calidad_recepcion_daños
integer x = 1783
integer y = 1064
integer width = 325
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Categorias"
boolean focusrectangle = false
end type

type dw_categoria from datawindow within w_info_control_calidad_recepcion_daños
integer x = 2245
integer y = 1044
integer width = 887
integer height = 92
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_categorias"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null

SetNull(li_Null)

IF NOT iuo_Categorias.Existe(Integer(data),True,SqlCa) THEN
	This.SetItem(1, "cate_codigo", li_Null)
	This.SetFocus()
	RETURN 1
END IF

end event

event itemerror;RETURN 1
end event

type cbx_categorias from checkbox within w_info_control_calidad_recepcion_daños
integer x = 2245
integer y = 960
integer width = 283
integer height = 76
integer taborder = 150
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todas"
boolean checked = true
end type

event clicked;String	ls_Null

SetNull(ls_Null)

IF This.Checked THEN
	dw_categoria.SetItem(1,"cate_codigo",Integer(ls_Null))
	dw_categoria.Enabled			=	False
	dw_categoria.Object.cate_codigo.BackGround.Color	=	RGB(192, 192, 192)
ELSE
	dw_categoria.Enabled		=	True
	dw_categoria.Object.cate_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_categoria.SetFocus()
END IF
end event

type cbx_camaracons from checkbox within w_info_control_calidad_recepcion_daños
integer x = 1106
integer y = 968
integer width = 443
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Consolidada"
end type

type st_1 from statictext within w_info_control_calidad_recepcion_daños
integer x = 1783
integer y = 1504
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Predio"
boolean focusrectangle = false
end type

type dw_predio from datawindow within w_info_control_calidad_recepcion_daños
integer x = 2245
integer y = 1492
integer width = 887
integer height = 92
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_predio"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Null

SetNull(li_Null)


ii_Predio	=	Integer(Data)
IF NOT iuo_Prodcuartel.Existe(il_Productor,ii_Predio,  + &
								 Integer(data),True,SqlCa) THEN
	This.SetItem(1, "prpr_codpre", li_Null)
	This.SetFocus()
	RETURN 1
ELSE
	dw_ccosto.GetChild("prcc_codigo",idwc_ccosto)
	idwc_ccosto.SetTransObject(Sqlca)
	dw_ccosto.Reset()
	dw_ccosto.InsertRow(0)
	idwc_ccosto.Retrieve(il_Productor,ii_Predio)
END IF

end event

type cbx_predio from checkbox within w_info_control_calidad_recepcion_daños
integer x = 2245
integer y = 1412
integer width = 283
integer height = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;String	ls_Null

SetNull(ls_Null)

IF This.Checked THEN
	dw_predio.SetItem(1,"prbr_codpre",Integer(ls_Null))
	cbx_predio.Enabled	=	True
	dw_predio.Enabled		=	False
	dw_predio.Object.prbr_codpre.BackGround.Color	=	RGB(192, 192, 192)
ELSE
	cbx_predio.Enabled	=	False
	cbx_predio.Checked	=	False
	dw_predio.Enabled			=	True
	dw_predio.Object.prbr_codpre.BackGround.Color	=	RGB(255, 255, 255)
	dw_predio.SetFocus()
END IF

end event

type dw_ccosto from datawindow within w_info_control_calidad_recepcion_daños
integer x = 2245
integer y = 1680
integer width = 887
integer height = 92
integer taborder = 130
boolean bringtotop = true
string dataobject = "dddw_centro_costo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Null

SetNull(li_Null)


ii_Ccosto	=	Integer(Data)
IF NOT iuo_CentroCostos.Existe(SqlCa,True,il_Productor,ii_Predio,  + &
								 Integer(data)) THEN
	This.SetItem(1, "prcc_codigo", li_Null)
	This.SetFocus()
	RETURN 1
END IF

end event

type st_2 from statictext within w_info_control_calidad_recepcion_daños
integer x = 1783
integer y = 1692
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Cuartel"
boolean focusrectangle = false
end type

type cbx_ccosto from checkbox within w_info_control_calidad_recepcion_daños
integer x = 2245
integer y = 1604
integer width = 283
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;String	ls_Null

SetNull(ls_Null)

IF This.Checked THEN
	dw_ccosto.SetItem(1,"prcc_codigo",Integer(ls_Null))
	cbx_ccosto.Enabled	=	True
	dw_ccosto.Enabled		=	False
	dw_ccosto.Object.prcc_codigo.BackGround.Color	=	RGB(192, 192, 192)
ELSE
	cbx_ccosto.Enabled	=	False
	cbx_ccosto.Checked	=	False
	dw_ccosto.Enabled		=	True
	dw_ccosto.Object.prcc_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_ccosto.SetFocus()
END IF

end event

type st_15 from statictext within w_info_control_calidad_recepcion_daños
integer x = 1778
integer y = 240
integer width = 544
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Fecha de Recepcion"
boolean focusrectangle = false
end type

type st_17 from statictext within w_info_control_calidad_recepcion_daños
integer x = 1792
integer y = 352
integer width = 197
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Desde"
boolean focusrectangle = false
end type

type st_18 from statictext within w_info_control_calidad_recepcion_daños
integer x = 2523
integer y = 352
integer width = 174
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Hasta"
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_control_calidad_recepcion_daños
integer x = 2112
integer y = 344
integer width = 329
integer height = 80
integer taborder = 30
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
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type em_hasta from editmask within w_info_control_calidad_recepcion_daños
integer x = 2798
integer y = 344
integer width = 329
integer height = 80
integer taborder = 50
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
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_19 from statictext within w_info_control_calidad_recepcion_daños
integer x = 169
integer y = 324
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_control_calidad_recepcion_daños
integer x = 443
integer y = 340
integer width = 1175
integer height = 92
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean minbox = true
boolean border = false
boolean livescroll = true
borderstyle borderstyle = styleraised!
end type

event itemchanged;ii_cliente = Integer(Data)
end event

type st_14 from statictext within w_info_control_calidad_recepcion_daños
integer x = 78
integer y = 204
integer width = 1600
integer height = 300
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_20 from statictext within w_info_control_calidad_recepcion_daños
integer x = 1682
integer y = 204
integer width = 1531
integer height = 300
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

