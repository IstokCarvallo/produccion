$PBExportHeader$w_info_recepcion_fruta_granel.srw
$PBExportComments$Ventana de Informe de Recepción de Fruta Granel.
forward
global type w_info_recepcion_fruta_granel from w_para_informes
end type
type dw_1 from datawindow within w_info_recepcion_fruta_granel
end type
type rb_inf_a from radiobutton within w_info_recepcion_fruta_granel
end type
type rb_inf_b from radiobutton within w_info_recepcion_fruta_granel
end type
type rb_inf_c from radiobutton within w_info_recepcion_fruta_granel
end type
type rb_inf_d from radiobutton within w_info_recepcion_fruta_granel
end type
type rb_pcev from radiobutton within w_info_recepcion_fruta_granel
end type
type gb_3 from groupbox within w_info_recepcion_fruta_granel
end type
end forward

global type w_info_recepcion_fruta_granel from w_para_informes
integer x = 14
integer y = 32
integer width = 5230
integer height = 1836
string title = "Recepción Fruta Granel"
string icon = "\Desarrollo\Produccion_3-2\ProdFrutaGranel\ProdFGranel.ico"
dw_1 dw_1
rb_inf_a rb_inf_a
rb_inf_b rb_inf_b
rb_inf_c rb_inf_c
rb_inf_d rb_inf_d
rb_pcev rb_pcev
gb_3 gb_3
end type
global w_info_recepcion_fruta_granel w_info_recepcion_fruta_granel

type variables
//
uo_plantadesp			iuo_plantadesp
uo_camarasfrigo		iuo_Camarasfrigo
uo_especie				iuo_especie
uo_grupoespecie		iuo_grupoespecie
uo_subgrupoespecie	iuo_subgrupoespecie
uo_variedades			iuo_variedades
uo_tratamientofrio		iuo_tratamientofrio
uo_periodofrio			iuo_periodofrio
uo_categorias			iuo_categorias
uo_productores			iuo_productores
uo_predios           		iuo_predios
uo_centrocostos      	iuo_centrocosto
uo_zonas				iuo_zonas
uo_ProdCuarteles		iuo_Cuartel
uo_ProdPredio			iuo_ProdPredio

DataWindowChild		idwc_planta, idwc_frigorifico, idwc_categorias, &
							idwc_camaras, idwc_especie, idwc_grupo, idwc_subgrupo, &
							idwc_variedad, idwc_tratamiento, idwc_periodo, &
							idwc_productor, idwc_ccosto, idwc_tipoenvase, &
							idwc_exporta, idwc_movto, idwc_predio, idwc_Cuartel, &
							idwc_certificacion


end variables

forward prototypes
public function boolean noexistetipoenvase (integer ai_tipo_enva)
public subroutine habilitatotal (integer ai_informe)
end prototypes

public function boolean noexistetipoenvase (integer ai_tipo_enva);String ls_tipo

SELECT	tien_nombre
	INTO	:ls_tipo
	FROM	dbo.tiposenvases
	WHERE	enva_tipoen	=	:ai_tipo_enva ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla TiposEnvases")
	
	RETURN TRUE
ELSEIF sqlca.SQLCode = -1 THEN
	MessageBox("Atención", "Código de Tipo Envase (" + String(ai_tipo_enva) + &
					"), no ha sido creado en tabla respectiva.~r~r" + &
					"Ingrese o seleccione otro Código.")
	
	RETURN TRUE
END IF

RETURN FALSE
end function

public subroutine habilitatotal (integer ai_informe);IF ai_informe = 1 OR ai_informe = 2 THEN
	dw_1.Object.totalfrigo.visible = 1
	dw_1.Object.totalcama.visible = 1
	dw_1.Object.totalespe.visible = 1
	dw_1.Object.totalvari.visible = 1
	dw_1.Object.totalgrup.visible = 0
	dw_1.Object.totalsubg.visible = 0
	dw_1.Object.totaltrat.visible = 0
	dw_1.Object.totalperi.visible = 1	
	dw_1.Object.totalprod.visible = 1		
	dw_1.Object.totallote.visible = 1
	dw_1.Object.TotalCate.Visible = 0
	
ELSEIF ai_informe = 3 THEN
	dw_1.Object.totalfrigo.visible = 1
	dw_1.Object.totalcama.visible = 1
	dw_1.Object.totalespe.visible = 1
	dw_1.Object.totalvari.visible = 1
	dw_1.Object.totalgrup.visible = 0
	dw_1.Object.totalsubg.visible = 0
	dw_1.Object.totaltrat.visible = 0
	dw_1.Object.totalperi.visible = 1	
	dw_1.Object.totalprod.visible = 0		
	dw_1.Object.totallote.visible = 1
	dw_1.Object.TotalCate.Visible = 0
	
ELSEIF ai_informe = 4 THEN
	dw_1.Object.totalfrigo.visible = 0
	dw_1.Object.totalcama.visible = 0
	dw_1.Object.totalespe.visible = 1
	dw_1.Object.totalvari.visible = 1
	dw_1.Object.totalgrup.visible = 1
	dw_1.Object.totalsubg.visible = 1
	dw_1.Object.totaltrat.visible = 1
	dw_1.Object.totalperi.visible = 0	
	dw_1.Object.totalprod.visible = 0		
	dw_1.Object.totallote.visible = 0		
	dw_1.Object.TotalZona.Visible = 1
	dw_1.Object.TotalCate.Visible = 0
	
ELSEIF ai_informe = 5 THEN
	dw_1.Object.totalfrigo.visible = 0
	dw_1.Object.totalcama.visible = 0
	dw_1.Object.totalespe.visible = 1
	dw_1.Object.totalvari.visible = 1
	dw_1.Object.totalgrup.visible = 0
	dw_1.Object.totalsubg.visible = 0
	dw_1.Object.totaltrat.visible = 0
	dw_1.Object.totalperi.visible = 0	
	dw_1.Object.totalprod.visible = 1		
	dw_1.Object.totallote.visible = 0
	dw_1.Object.TotalZona.Visible = 0
	dw_1.Object.TotalCate.Visible = 1
	
END IF	
end subroutine

on w_info_recepcion_fruta_granel.create
int iCurrent
call super::create
this.dw_1=create dw_1
this.rb_inf_a=create rb_inf_a
this.rb_inf_b=create rb_inf_b
this.rb_inf_c=create rb_inf_c
this.rb_inf_d=create rb_inf_d
this.rb_pcev=create rb_pcev
this.gb_3=create gb_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
this.Control[iCurrent+2]=this.rb_inf_a
this.Control[iCurrent+3]=this.rb_inf_b
this.Control[iCurrent+4]=this.rb_inf_c
this.Control[iCurrent+5]=this.rb_inf_d
this.Control[iCurrent+6]=this.rb_pcev
this.Control[iCurrent+7]=this.gb_3
end on

on w_info_recepcion_fruta_granel.destroy
call super::destroy
destroy(this.dw_1)
destroy(this.rb_inf_a)
destroy(this.rb_inf_b)
destroy(this.rb_inf_c)
destroy(this.rb_inf_d)
destroy(this.rb_pcev)
destroy(this.gb_3)
end on

event open;call super::open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp
iuo_especie				=	Create uo_especie
iuo_grupoespecie		=	Create uo_grupoespecie
iuo_subgrupoespecie	=	Create uo_subgrupoespecie
iuo_variedades			=	Create uo_variedades
iuo_tratamientofrio	=	Create uo_tratamientofrio
iuo_periodofrio		=	Create uo_periodofrio
iuo_categorias			=	Create uo_categorias
iuo_productores		=	Create uo_productores
iuo_predios				=	Create uo_predios
iuo_centrocosto		=	Create uo_centrocostos
iuo_Camarasfrigo		=	Create uo_camarasfrigo	
iuo_zonas				=	Create uo_zonas	
iuo_Cuartel				=	Create uo_ProdCuarteles
iuo_ProdPredio			=	Create uo_ProdPredio

//Exportador
dw_1.GetChild("cliente", idwc_exporta)
idwc_exporta.SetTransObject(sqlca)
IF idwc_exporta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Exporta")
	idwc_exporta.InsertRow(0)
END IF

//Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

//Movimiento
dw_1.GetChild("Movimiento", idwc_movto)
idwc_movto.SetTransObject(sqlca)
IF idwc_movto.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Movimientos")
	idwc_Movto.InsertRow(0)
ELSE
	idwc_movto.SetFilter("tpmv_tipcor = 1" + " and tpmv_frugra = 1")
	idwc_movto.Filter()
END IF

//Frigorifico
dw_1.GetChild("frigo", idwc_frigorifico)
idwc_frigorifico.SetTransObject(sqlca)
idwc_frigorifico.InsertRow(0)

//Camara Frigorifico
dw_1.GetChild("camara", idwc_camaras)
idwc_camaras.SetTransObject(sqlca)
idwc_camaras.InsertRow(0)


//Especie
dw_1.GetChild("especie", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
END IF

//Variedad
dw_1.GetChild("variedad",idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
IF idwc_variedad.Retrieve(0) = 0 THEN
	//MessageBox("Atención","Falta Registrar Variedades")
	idwc_variedad.InsertRow(0)
END IF

//Grupo
dw_1.GetChild("grupo",idwc_grupo)
idwc_grupo.SetTransObject(Sqlca)
IF idwc_grupo.Retrieve(0,0) = 0 THEN
	MessageBox("Atención","Falta Registrar Grupos")
	idwc_grupo.InsertRow(0)
END IF

//Sub Grupo
dw_1.GetChild("subgrupo",idwc_subgrupo)
idwc_subgrupo.SetTransObject(Sqlca)
IF idwc_subgrupo.Retrieve(0,0) = 0 THEN
	MessageBox("Atención","Falta Registrar Sub Grupos")
	idwc_subgrupo.InsertRow(0)
END IF

//Tratamiento
dw_1.GetChild("tratamiento",idwc_tratamiento)
idwc_tratamiento.SetTransObject(Sqlca)
IF idwc_tratamiento.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Tratamientos")
	idwc_tratamiento.InsertRow(0)
END IF

//Periodo
dw_1.GetChild("periodo",idwc_periodo)
idwc_periodo.SetTransObject(Sqlca)
IF idwc_periodo.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Periodos")
	idwc_periodo.InsertRow(0)
END IF

//Categorias
dw_1.GetChild("categoria",idwc_categorias)
idwc_categorias.SetTransObject(Sqlca)
IF idwc_periodo.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Categorias")
	idwc_categorias.InsertRow(0)
END IF
idwc_categorias.Setfilter('isNull( cate_embala ) or  cate_embala <> 1')
idwc_categorias.Filter()

//Productor
dw_1.GetChild("productor",idwc_productor)
idwc_productor.SetTransObject(Sqlca)
IF idwc_productor.Retrieve(-1) = 0 THEN
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
END IF

//Tipo de Envase
dw_1.GetChild("tiposenvase",idwc_tipoenvase)
idwc_tipoenvase.SetTransObject(Sqlca)
IF idwc_tipoenvase.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Tipos de Envase")
	idwc_tipoenvase.InsertRow(0)
END IF

////Predio
//dw_1.GetChild("predio",idwc_predio)
//idwc_predio.SetTransObject(Sqlca)
//IF idwc_predio.Retrieve(0) = 0 THEN
//	//MessageBox("Atención","Falta Registrar Predios")
//	idwc_predio.InsertRow(0)
//END IF

//Centro Costo
dw_1.GetChild("centrocosto",idwc_ccosto)
idwc_ccosto.SetTransObject(Sqlca)
IF idwc_ccosto.Retrieve(0,0) = 0 THEN
	//MessageBox("Atención","Falta Registrar Centros de Costo")
	idwc_ccosto.InsertRow(0)
END IF

dw_1.GetChild("predio", idwc_predio)
idwc_predio.SetTransObject(sqlca)

dw_1.GetChild("cuartel", idwc_Cuartel)
idwc_Cuartel.SetTransObject(sqlca)

dw_1.GetChild("mercado", idwc_certificacion)
idwc_certificacion.SetTransObject(sqlca)

							
dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

Paramtemporada(gstr_paramtempo)
dw_1.SetItem(1,"fechad", gstr_paramtempo.fechainicio)
dw_1.SetItem(1,"fechah", Today())
dw_1.setitem(1,"cliente", gi_codexport)

habilitatotal(1)

end event

type pb_excel from w_para_informes`pb_excel within w_info_recepcion_fruta_granel
end type

type st_computador from w_para_informes`st_computador within w_info_recepcion_fruta_granel
integer x = 1006
integer width = 4206
end type

type st_usuario from w_para_informes`st_usuario within w_info_recepcion_fruta_granel
integer x = 1006
integer width = 4206
end type

type st_temporada from w_para_informes`st_temporada within w_info_recepcion_fruta_granel
integer x = 1006
integer width = 4206
end type

type p_logo from w_para_informes`p_logo within w_info_recepcion_fruta_granel
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_recepcion_fruta_granel
integer width = 4503
integer height = 76
integer textsize = -9
string text = "Informe Recepción de Fruta Granel"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_recepcion_fruta_granel
integer x = 4800
integer y = 868
integer height = 224
integer taborder = 370
end type

event pb_acepta::clicked;
Integer	li_Planta, li_Especie, li_Grupo, li_SubGrupo, li_Predio, li_Ccosto, &
			li_Variedad, li_Periodo, li_Categoria, li_tipoenvase, &
			li_ConsLote, li_Consfechad, li_exporta, li_Movimiento, li_zona, &
			li_cuartel, li_condmerc, li_kilos
Long		ll_Fila, ll_Productor, ll_Frigo, ll_Camara
Date		ld_Fechadesde, ld_FechaHasta, ld_fechaprueba
String	ls_Tratamiento, ls_fecha, ls_titulo

SetPointer(Arrow!)
 
istr_info.titulo	= 'RECEPCION DE FRUTA GRANEL'

OpenWithParm(vinf, istr_info)

IF rb_inf_a.checked THEN
	vinf.dw_1.DataObject = "dw_info_recepcion_fruta_granel_a"
	ls_titulo = rb_inf_a.Text
ELSEIF rb_inf_b.checked THEN
	vinf.dw_1.DataObject = "dw_info_recepcion_fruta_granel_b"
	ls_titulo = rb_inf_b.Text
	vinf.dw_1.Object.DataWindow.Zoom = 95
ELSEIF rb_inf_c.checked THEN
	vinf.dw_1.DataObject = "dw_info_recepcion_fruta_granel_c"
	ls_titulo = rb_inf_c.Text
ELSEIF rb_inf_d.checked THEN
	vinf.dw_1.DataObject = "dw_info_recepcion_fruta_granel"
	ls_titulo = rb_inf_d.Text
ELSEIF rb_pcev.checked THEN
	vinf.dw_1.DataObject = "dw_info_recepcion_fruta_granel_pcev"
	ls_titulo = rb_inf_d.Text
END IF
dw_1.accepttext()

// Acepta Exportador
li_exporta = dw_1.Object.cliente[1]
IF IsNull(li_exporta) Then
	MessageBox( "Exportador Erróneo", "Falta seleccionar un Exportador.", &
				 StopSign!, Ok!)
	RETURN				 
END IF

// Acepta Movimiento //
IF dw_1.Object.todosmov[1] = 1 THEN
	IF dw_1.Object.exclrecemb[1] = 1 THEN
		li_movimiento = 2000
	ELSE
		li_movimiento = 1000
	END IF	
ELSE
	li_Movimiento = dw_1.Object.Movimiento[1]
	IF IsNull(li_Movimiento) Then
		MessageBox( "Movimiento Erróneo", "Falta seleccionar un Movimiento.", &
	             StopSign!, Ok!)
		RETURN				 
   END IF
END IF

// Acepta Planta //
IF dw_1.Object.todosplanta[1] = 1 THEN
	li_Planta = 10000
ELSE
	li_Planta = dw_1.Object.planta[1]
	IF IsNull(li_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", &
	             StopSign!, Ok!)
		RETURN				 
   END IF
END IF

// Acepta Frigorifico //
IF dw_1.Object.TodosFrigo[1] = 1 THEN
	ll_Frigo = 10000
	IF dw_1.Object.consfrigo[1] = 1 THEN
		ll_Frigo = 90000
	END IF
ELSE
	ll_Frigo	= dw_1.Object.frigo[1]
	IF IsNull(ll_Frigo) THEN
		MessageBox( "Frigorifico Erróneo", "Falta seleccionar un Frigorifico.", &
	             StopSign!, Ok!)
		RETURN				 
   END If
END IF	

// Acepta Camara //
IF dw_1.Object.todoscamara[1] = 1 THEN
	ll_Camara = 10000
	IF dw_1.Object.conscamara[1] = 1 THEN	
		ll_Camara	=	90000
	END IF
ELSE
	ll_Camara	=	dw_1.Object.camara[1]
	IF IsNull(ll_Camara) THEN
		MessageBox( "Camara Errónea", "Falta seleccionar una Camara.", &
	             StopSign!, Ok!)
		RETURN				 
   END If
END IF

// Acepta Especie //
IF dw_1.Object.todosespe[1] = 1 THEN
	li_Especie = 100
ELSE
	li_Especie = dw_1.Object.especie[1]
	If IsNull(li_especie) Then
		MessageBox( "Especie Errónea", "Falta seleccionar una Especie.", &
	             StopSign!, Ok!)
		RETURN				 
   END If
END IF

// Acepta Variedad //
IF dw_1.Object.todosvari[1] = 1 THEN
	li_Variedad = 10000
ELSE
	li_Variedad = dw_1.Object.variedad[1]
	If IsNull(li_Variedad) Then
		MessageBox( "Variedad Errónea", "Falta seleccionar una Variedad.", &
	             StopSign!, Ok!)
		RETURN			 
   END If
END IF

// Acepta Grupo de Variedades //
IF dw_1.Object.todosvari[1] = 1 THEN  
	IF dw_1.Object.todosgrupo[1] = 1  THEN
		li_Grupo = 100
		IF dw_1.Object.consgrupo[1] = 1  THEN	
			li_Grupo =	900
		END IF
	ELSE
		li_Grupo = dw_1.Object.grupo[1]
		If IsNull(li_Grupo) Then
			MessageBox( "Grupo Erróneo", "Falta seleccionar un Grupo de Especie.", &
	      	       StopSign!, Ok!)
			RETURN				 
   	END IF
	END IF
ELSE
	li_Grupo = 100
END IF

// Acepta SubGrupo de Variedades //
IF dw_1.Object.todosvari[1] = 1 THEN 
	IF dw_1.Object.todossubgru[1] = 1  THEN
		li_SubGrupo = 100
		IF dw_1.Object.conssubgr[1] = 1 THEN
			li_SubGrupo=900
		END IF
	ELSE
		li_SubGrupo = dw_1.Object.subgrupo[1]
		If IsNull(li_SubGrupo) Then
			MessageBox( "SubGrupo Erróneo", "Falta seleccionar un SubGrupo de Especie.", &
					 StopSign!, Ok!)
			RETURN
		END If
	END IF
ELSE
	li_SubGrupo = 100
END IF

// Acepta Tratamientos de frio //
IF dw_1.Object.todostrata[1] = 1 THEN
	ls_Tratamiento = '*'
	IF dw_1.Object.constrata[1] = 1 THEN	
		ls_Tratamiento = '**'
	END IF
ELSE
	ls_Tratamiento = dw_1.Object.tratamiento[1]
	If IsNull(ls_Tratamiento) or ls_tratamiento="" Then
		MessageBox( "Tratamiento Erróneo", "Falta seleccionar un Tratamiento.", &
	             StopSign!, Ok!)
		RETURN				 
   END If
END IF

// Acepta Periodo de Frio //
IF dw_1.Object.todosperio[1] = 1 THEN
	li_Periodo = 100
	IF dw_1.Object.consperio[1] = 1 THEN	
		li_Periodo = 900
	END IF
ELSE
	li_Periodo = dw_1.Object.periodo[1]
	If IsNull(li_Periodo) Then
		MessageBox( "Período Erróneo", "Falta seleccionar un Período.", &
	             StopSign!, Ok!)
		RETURN				 
   END If
END IF

// Acepta Categorias
IF dw_1.Object.todoscate[1] = 1  THEN
	li_Categoria = 1000
ELSE
	li_Categoria = dw_1.Object.categoria[1]
	If IsNull(li_Categoria) Then
		MessageBox( "Categoria Errónea", "Falta seleccionar una Categoria.", &
	             StopSign!, Ok!)
		RETURN				 
   END If
END IF

// Acepta Productor
IF dw_1.Object.todosprod[1] = 1 THEN
	ll_Productor = 10000
	IF dw_1.Object.consprod[1] = 1 THEN	
		ll_Productor =	90000
	END IF
ELSE
	ll_Productor = dw_1.Object.productor[1]
	If IsNull(ll_Productor) Then
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.", &
	             StopSign!, Ok!)
		RETURN				 
   END If
END IF

// Acepta Predio
IF dw_1.Object.todospredio[1] = 1 THEN
	li_Predio = -1
	IF dw_1.Object.conspredio[1] = 1 THEN	
		li_Predio =	900
	END IF
ELSE
	li_Predio = dw_1.Object.predio[1]
	If IsNull(li_Predio) Then
		MessageBox( "Predio Erróneo", "Falta seleccionar un Predio.", &
	             StopSign!, Ok!)
		RETURN				 
   END If
END IF

//Acepta Cuartel
IF dw_1.Object.todoscuartel[1] = 1 THEN
	li_Cuartel = -1
ELSE
	li_Cuartel  = dw_1.Object.cuartel[1]
	If IsNull(li_Cuartel ) Then
		MessageBox( "Cuartel Erróneo", "Falta seleccionar un Cuartel.", &
	             StopSign!, Ok!)
		RETURN				 
   END If
END IF

//Acepta condicion de mercado
IF dw_1.Object.todosmercado[1] = 1 THEN
	li_condmerc = -1
ELSE
	li_condmerc  = dw_1.Object.mercado[1]
	If IsNull(li_condmerc ) Then
		MessageBox( "Mercado Erróneo", "Falta seleccionar un Mercado.", &
	             StopSign!, Ok!)
		RETURN				 
   END If
END IF

// Acepta Centro Costo
IF dw_1.Object.todosccosto[1] = 1 THEN
	li_ccosto = 100
	IF dw_1.Object.conspredio[1] = 1 THEN	
		li_ccosto =	900
	END IF
ELSE
	li_ccosto = dw_1.Object.centrocosto[1]
	If IsNull(li_ccosto) Then
		MessageBox( "Centro Costo Erróneo", "Falta seleccionar un Centro de Costo.", &
	             StopSign!, Ok!)
		RETURN				 
   END If
END IF

// Acepta Todos Lotes o Consolida
IF dw_1.Object.consollote[1] = 1 THEN
		li_ConsLote =	1
	ELSE
		li_ConsLote = 0
END IF

// Acepta Tipos de Envase //
IF dw_1.Object.todostiposenvase[1] = 1 THEN
	 li_tipoenvase = 10
ELSE
	 li_tipoenvase = dw_1.Object.tiposenvase[1]
	If IsNull( li_tipoenvase) Then
		MessageBox( "Tipo de Envase Erróneo", "Falta seleccionar un Tipo de Envase.", &
	             StopSign!, Ok!)
		RETURN
   END If
END IF

IF dw_1.Object.todoszona[1] = 1 THEN
	li_zona = -1
ELSE
	li_zona= dw_1.Object.zona[1]
	If IsNull( li_zona ) Then
		MessageBox( "Código de Zona Erroneo", "Falta seleccionar un Código de Zona.", &
	             StopSign!, Ok!)
		RETURN
   END If
END IF

ld_fechadesde = dw_1.Object.fechad[1]

If IsNull(ld_fechadesde) or ld_fechadesde <= ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN			 
END If

ls_fecha=mid(String(dw_1.Object.fechah[1],'dd/mm/yyyy'),1,10)
ld_fechahasta = Date(ls_fecha)

If IsNull(ld_fechahasta) or ld_fechahasta<=ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", &
	             StopSign!, Ok!)
	RETURN				 
END If

IF ld_fechadesde > ld_fechahasta THEN
	MessageBox( "Fechas Erroneas", "La Fecha Hasta debe ser mayor o igual a la Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN
END IF	

IF dw_1.Object.consfecha[1]= 1 THEN	
	li_consfechad	 =		1
ELSE
	li_Consfechad	 =		0
END IF

IF dw_1.Object.kilos[1]= 1 THEN	
	li_kilos			=		1
ELSE
	li_kilos			=		0
END IF

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_Movimiento,li_Planta,ll_Frigo,ll_Camara,&
									    li_Especie,li_Grupo,li_SubGrupo,li_Variedad,ls_Tratamiento, &
                               li_Periodo,ll_Productor, li_predio, li_ccosto, li_Categoria, &
										 li_tipoenvase, ld_fechadesde, ld_fechahasta,  &
										 li_consfechad, li_ConsLote, li_exporta, li_zona, li_Predio, &
										 li_Cuartel, li_condmerc, li_kilos)

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
	vinf.dw_1.Modify("t_titulo.text = '" + ls_titulo + "'")
	IF rb_inf_a.checked THEN
		IF dw_1.Object.totalespe[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.2.Height=0")
		IF dw_1.Object.totalfrigo[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.3.Height=0")
		IF dw_1.Object.totalcama[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.4.Height=0")
		IF dw_1.Object.totalperi[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.5.Height=0")	
		IF dw_1.Object.totalprod[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.6.Height=0")		
		IF dw_1.Object.totalvari[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.7.Height=0")
		IF dw_1.Object.totallote[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.8.Height=0")
	ELSEIF rb_inf_b.checked THEN
		IF dw_1.Object.totalespe[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.2.Height=0")
		IF dw_1.Object.totalfrigo[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.3.Height=0")
		IF dw_1.Object.totalcama[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.4.Height=0")
		IF dw_1.Object.totalperi[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.5.Height=0")	
		IF dw_1.Object.totalvari[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.6.Height=0")		
		IF dw_1.Object.totallote[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.7.Height=0")
		IF dw_1.Object.totalprod[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.8.Height=0")
	ELSEIF rb_inf_c.checked THEN
		IF dw_1.Object.totalespe[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.2.Height=0")
		IF dw_1.Object.totalperi[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.3.Height=0")
		IF dw_1.Object.totalvari[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.4.Height=0")
		IF dw_1.Object.totallote[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.5.Height=0")	
		IF dw_1.Object.totalfrigo[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.6.Height=0")		
		IF dw_1.Object.totalcama[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.7.Height=0")
	ELSEIF rb_inf_d.checked THEN
		IF dw_1.Object.totalespe[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.2.Height=0")
		IF dw_1.Object.totalgrup[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.3.Height=0")
		IF dw_1.Object.totalsubg[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.4.Height=0")
		IF dw_1.Object.totalvari[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.5.Height=0")	
		IF dw_1.Object.totaltrat[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.6.Height=0")		
	ELSEIF rb_pcev.checked THEN		
		IF dw_1.Object.totalprod[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.1.Height=0")			
		IF dw_1.Object.totalcate[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.2.Height=0")	
		IF dw_1.Object.totalespe[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.3.Height=0")	
		IF dw_1.Object.totalespe[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.4.Height=0")	
	END IF
			
	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_recepcion_fruta_granel
integer x = 4800
integer y = 1156
integer height = 224
integer taborder = 380
end type

type dw_1 from datawindow within w_info_recepcion_fruta_granel
integer x = 251
integer y = 628
integer width = 4503
integer height = 980
integer taborder = 60
string title = "none"
string dataobject = "dw_info_recepcion_frutagranel_seleccion"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;call super::itemchanged;Integer	li_Null, li_cliente
String	ls_Columna

SetNull(li_Null)
dw_1.accepttext()


li_cliente = dw_1.Object.cliente[1]

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
		
	CASE "planta"
		IF NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) THEN
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			RETURN 1
		END IF		
		
		//GetChild Frigorifico
		IF idwc_frigorifico.Retrieve(Integer(data)) = 0 THEN
			MessageBox("Atención", "Falta Registrar las camaras")
			idwc_frigorifico.InsertRow(0)
		ELSE
			idwc_frigorifico.SetFilter("cama_codigo = cama_codfri")
			idwc_frigorifico.Filter()
		END IF

		//GetChild Camara
		IF idwc_camaras.Retrieve(Integer(data)) = 0 THEN
			MessageBox("Atención","Falta Registrar Cámaras Frigorífico Seleccionado")
			idwc_camaras.InsertRow(0)
		END IF
		
	CASE "frigo"
		IF Not iuo_Camarasfrigo.Existe(integer(dw_1.Object.Planta[1]), Integer(data), True, Sqlca) THEN
			This.SetItem(1, "frigo", li_Null )
			This.SetFocus()
			RETURN 1
		ElSE
			dw_1.Object.todoscamara.protect = 0
		END IF		

		idwc_camaras.SetFilter("cama_codfri = " + data + " And cama_codfri <> cama_codigo") 
		idwc_camaras.Filter()

	CASE "camara"
		IF NOT iuo_CamarasFrigo.Existe(This.Object.planta[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "camara", li_Null )
			This.SetFocus()
			RETURN 1
		Else
			dw_1.Object.todoscamara.protect = 0
		END IF		
		

	CASE "todosplanta"
		IF data='0' THEN
			dw_1.Object.todoscamara.protect = 1
		ELSEIF data = '1' THEN 
			This.Object.planta[row]			= li_Null
			dw_1.Object.frigo[row]	= li_Null
			dw_1.Object.todosfrigo[row]	= 1
			dw_1.Object.camara[row]			= li_Null
			dw_1.Object.todoscamara[row]		= 1
		END IF
		
	CASE "todosfrigo"
		IF data = '0' THEN
			dw_1.Object.todoscamara.protect = 1
		ELSEIF data = '1' THEN
			This.Object.frigo[row]	= li_Null
			dw_1.Object.camara[row]			= li_Null
			dw_1.Object.todoscamara[row]		= 1
		END IF
 
	CASE "todoscama"
		IF data='1' THEN 
			This.Object.camara[row]	= li_Null
			dw_1.Object.camara[row]	=	li_Null
		END IF
	


	CASE "tiposenvase"
		IF noexistetipoenvase(integer(data)) THEN
			This.SetItem(1, "tiposenvase", li_Null )
			This.SetFocus()
			RETURN 1
		END IF	

	CASE "todostiposenvase"
	  IF data = '1' THEN This.Object.tiposenvase[row]	=	li_Null


	CASE "especie"
		This.Object.Variedad[row]	=	li_Null
		This.Object.Grupo[row]		=	li_Null
		This.Object.SubGrupo[row]	=	li_Null
		IF NOT iuo_Especie.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "especie", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			idwc_grupo.settransobject(sqlca)
			idwc_grupo.Retrieve(Integer(data),0)
			idwc_subgrupo.settransobject(sqlca)
			idwc_subgrupo.Retrieve(Integer(data),0)
			idwc_variedad.settransobject(sqlca)
			idwc_variedad.Retrieve(Integer(data))
		END IF
	
  CASE	"todosespe"
		IF	data = '1' THEN
			This.Object.especie[row]		=	li_Null
			this.Object.variedad[row]		=	li_Null
			this.Object.grupo[row]			= 	li_Null
			this.Object.subgrupo[row]		= 	li_Null
			dw_1.Object.todosvari[row]		=	1
			dw_1.Object.todosgrupo[row]	=	1
			dw_1.Object.todossubgru[row]	=	1
		ELSE
			This.Object.especie[row]		=	li_Null
			This.Object.Variedad[row]		=	li_Null
			This.Object.Grupo[row]			=	li_Null
			This.Object.SubGrupo[row]		=	li_Null			
		END IF	
	

	CASE "variedad"
			This.Object.Grupo[row]		=	li_Null
			This.Object.SubGrupo[row]	=	li_Null
			
		IF NOT iuo_Variedades.Existe(This.Object.especie[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "variedad", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			idwc_grupo.settransobject(sqlca)
			idwc_grupo.Retrieve(This.Object.especie[row],Integer(data))
			idwc_subgrupo.settransobject(sqlca)
			idwc_subgrupo.Retrieve(This.Object.especie[row],Integer(data))
			
			This.Object.Grupo[row]			=	iuo_Variedades.Grupo
			This.Object.SubGrupo[row]		=	iuo_Variedades.SubGrupo
			
			dw_1.Object.todosgrupo.protect	=	1
			dw_1.Object.todossubgru.protect	=	1
			dw_1.Object.todosgrupo[1] 			=	0
			dw_1.Object.todossubgru[1] 		=	0
			dw_1.Object.consgrupo[1] 			=	0
			dw_1.Object.conssubgr[1]	 		=	0
			dw_1.Object.consgrupo.protect		=	1
			dw_1.Object.conssubgr.protect		=	1
		END IF
		
	CASE	"todosvari"
		IF	data = '1' THEN
			dw_1.Object.variedad[row]		=	li_Null
			dw_1.Object.grupo[row]			= 	li_Null
			dw_1.Object.subgrupo[row]		= 	li_Null
			dw_1.Object.todosgrupo[1] 		=	1
			dw_1.Object.todossubgru[1] 	=	1
		END IF
		
	CASE "grupo"
		IF NOT iuo_GrupoEspecie.Existe(This.Object.especie[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "grupo", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			idwc_subgrupo.Retrieve(This.Object.especie[row],Integer(data))
			This.Object.SubGrupo[row]	=	li_Null
		END IF
			
	CASE "subgrupo"
		IF NOT iuo_SubGrupoEspecie.Existe(This.Object.especie[row],&
					This.Object.grupo[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "subgrupo", li_Null )
			This.SetFocus()
			RETURN 1
		END IF
		
	CASE "tratamiento"
		IF NOT iuo_TratamientoFrio.Ofp_Recupera_TratamientoFrio(SqlCa,data,True) THEN
			This.SetItem(1, "tratamiento", String(li_Null))
			This.SetFocus()
			RETURN 1
		END IF

	CASE "periodo"
		IF NOT iuo_PeriodoFrio.Ofp_Recupera_PeriodoFrio(SqlCa,Integer(data),True) THEN
			This.SetItem(1, "periodo", li_Null)
			This.SetFocus()
			RETURN 1
		END IF
		
	CASE "categoria"
		IF NOT iuo_Categorias.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "categoria", li_Null)
			This.SetFocus()
			RETURN 1
		END IF

	CASE "predio"
		IF NOT iuo_ProdPredio.Existe(Integer(data),This.Object.productor[Row],&
											  True,SQLCA) THEN
			This.Object.predio[Row]	=	li_null
			RETURN 1
		ELSE		
			idwc_Cuartel.Retrieve(This.Object.productor[Row],Integer(data))
			idwc_certificacion.Retrieve(This.Object.productor[Row],Integer(data),This.Object.especie[row])
		END IF
		
	CASE "productor"
		IF NOT iuo_Productores.Existe(Long(data),True,SqlCa) THEN
			This.SetItem(1, "productor", li_Null)
			This.SetFocus()
			RETURN 1
		ELSE
			this.Object.todospredio.protect = 	0
			This.Object.predio[row]			  =	li_Null
			This.Object.centrocosto[row]	  =	li_Null
			idwc_Predio.Retrieve(Long(Data))
			idwc_Cuartel.Reset()
		END IF
		
	CASE "zona"
		IF NOT iuo_zonas.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "zona", li_Null)
			This.SetFocus()
			RETURN 1
		END IF
		
	CASE "cuartel"
		IF NOT iuo_Cuartel.Existe(This.Object.productor[Row],&
									  This.Object.predio[Row],&
									  Integer(data),True,SQLCA) THEN
		This.Object.cuartel[Row]	=	li_null
		RETURN 1
	ELSE		
		This.Object.Especie[Row]	=	iuo_Cuartel.Especie
		This.Object.variedad[Row]	=	iuo_Cuartel.Variedad
	END IF
		
   CASE "centrocosto"
		IF NOT iuo_centrocosto.Existe(Sqlca,True,dw_1.Object.productor[1],dw_1.Object.predio[1],Integer(data)) THEN
			This.SetItem(1, "centrocosto", li_Null )
			This.SetFocus()
			RETURN 1
		END IF		
		
	
	
	CASE	"todostrata"
		IF	data = '1' THEN This.Object.tratamiento[row]	=	String(li_Null)
	
	CASE	"todosperio"
		IF	data = '1' THEN This.Object.periodo[row]		=	li_Null
	
	CASE	"todoscate"
		IF	data = '1' THEN This.Object.categoria[row]	=	li_Null
		
	CASE "todoszona"
		IF	data = '1' THEN This.Object.Zona[row]			=	li_Null
		
	CASE	"todosprod"
		IF data='0' THEN
			dw_1.Object.Todospredio[1]			=	1	
			dw_1.Object.TodosCCosto[1] 		= 	1
			dw_1.Object.TodosPredio.protect 	= 	1
			dw_1.Object.TodosCCosto[1] 		= 	1
			dw_1.Object.TodosZona[1]			=	1
		ELSEIF	data = '1' THEN 
			dw_1.Object.TodosPredio.protect 	= 	0
			This.Object.productor[1]			=	li_Null
			This.Object.predio[1]				=	li_Null
			This.Object.centrocosto[1]			=	li_Null
			This.Object.Zona[1]					=	li_Null
		END IF
		
	CASE	"todospredio"
		IF data='0' THEN
			dw_1.Object.todosccosto.protect = 1
		ELSEIF	data = '1' THEN 
			This.Object.predio[row]			=	li_Null
			This.Object.centrocosto[row]	=	li_Null
		END IF	
	
	CASE	"todosccosto"
		IF	data = '1' THEN This.Object.centrocosto[row]	=	li_Null
		
END CHOOSE
end event

event itemerror;RETURN 1
end event

type rb_inf_a from radiobutton within w_info_recepcion_fruta_granel
integer x = 347
integer y = 412
integer width = 1641
integer height = 60
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Especie/ Frigorífico/Cámara/Período/Productor/Variedad/Lote"
boolean checked = true
end type

event clicked;habilitatotal(1)
end event

type rb_inf_b from radiobutton within w_info_recepcion_fruta_granel
integer x = 347
integer y = 472
integer width = 1641
integer height = 60
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Especie/ Frigorífico/Cámara/Período/Variedad/Lote/Productor"
end type

event clicked;habilitatotal(2)
end event

type rb_inf_c from radiobutton within w_info_recepcion_fruta_granel
integer x = 347
integer y = 532
integer width = 1641
integer height = 60
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Especie/Período/Variedad/Frigorífico/Cámara"
end type

event clicked;habilitatotal(3)
end event

type rb_inf_d from radiobutton within w_info_recepcion_fruta_granel
integer x = 2633
integer y = 412
integer width = 1641
integer height = 68
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Especie/Grupo/Subgrupo/Variedad/Tipo Frio"
end type

event clicked;habilitatotal(4)
end event

type rb_pcev from radiobutton within w_info_recepcion_fruta_granel
integer x = 2633
integer y = 472
integer width = 1641
integer height = 56
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Productor/Categoria/Especie/Variedad"
end type

event clicked;HabilitaTotal(5)
end event

type gb_3 from groupbox within w_info_recepcion_fruta_granel
integer x = 251
integer y = 376
integer width = 4503
integer height = 232
integer taborder = 10
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Ordenado Por"
end type

