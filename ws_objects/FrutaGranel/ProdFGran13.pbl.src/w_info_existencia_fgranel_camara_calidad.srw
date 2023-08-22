$PBExportHeader$w_info_existencia_fgranel_camara_calidad.srw
$PBExportComments$Ventana Informe de Existencia de Fruta Granel en Camara.
forward
global type w_info_existencia_fgranel_camara_calidad from w_para_informes
end type
type rb_inf_a from radiobutton within w_info_existencia_fgranel_camara_calidad
end type
type rb_inf_b from radiobutton within w_info_existencia_fgranel_camara_calidad
end type
type rb_inf_c from radiobutton within w_info_existencia_fgranel_camara_calidad
end type
type rb_inf_d from radiobutton within w_info_existencia_fgranel_camara_calidad
end type
type dw_1 from datawindow within w_info_existencia_fgranel_camara_calidad
end type
type dw_2 from datawindow within w_info_existencia_fgranel_camara_calidad
end type
type gb_3 from groupbox within w_info_existencia_fgranel_camara_calidad
end type
end forward

global type w_info_existencia_fgranel_camara_calidad from w_para_informes
integer width = 5417
integer height = 1944
string title = "Existencia Fruta Granel por Camara"
rb_inf_a rb_inf_a
rb_inf_b rb_inf_b
rb_inf_c rb_inf_c
rb_inf_d rb_inf_d
dw_1 dw_1
dw_2 dw_2
gb_3 gb_3
end type
global w_info_existencia_fgranel_camara_calidad w_info_existencia_fgranel_camara_calidad

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
uo_zonas					iuo_zonas
uo_ProdCuarteles			iuo_Cuartel
uo_ProdPredio			iuo_ProdPredio

DataWindowChild	idwc_planta, idwc_frigorifico, idwc_categorias, idwc_camaras, idwc_especie, &
						idwc_grupo, idwc_subgrupo, idwc_variedad, idwc_tratamiento, idwc_periodo, &
						idwc_productor, idwc_exporta,idwc_predio, idwc_Cuartel, idwc_certificacion

String	is_NomPlanta
Integer	ii_Planta, ii_Frigorifico, ii_Especie, ii_Grupo
end variables

forward prototypes
public subroutine habilitatotal (integer ai_informe)
end prototypes

public subroutine habilitatotal (integer ai_informe);IF ai_informe = 1 THEN
	dw_1.Object.totalfrigo.visible			=	1
	dw_1.Object.totalcama.visible				=	1
	dw_1.Object.totalband.visible				=	0
	dw_1.Object.totalespe.visible				=	1
	dw_1.Object.totalvari.visible				=	1
	dw_1.Object.totalgrup.visible				=	0
	dw_1.Object.totalsubg.visible				=	0
	dw_1.Object.totaltrat.visible				=	0
	dw_1.Object.totalperi.visible				=	1
	dw_1.Object.totalcate.visible				=	0
	dw_1.Object.totalprod.visible				=	1
	
ELSEIF ai_informe = 2 THEN
	dw_1.Object.totalfrigo.visible			=	1
	dw_1.Object.totalcama.visible				=	1
	dw_1.Object.totalband.visible				=	0
	dw_1.Object.totalespe.visible				=	1
	dw_1.Object.totalvari.visible				=	1
	dw_1.Object.totalgrup.visible				=	0
	dw_1.Object.totalsubg.visible				=	0
	dw_1.Object.totaltrat.visible				=	0
	dw_1.Object.totalperi.visible				=	1
	dw_1.Object.totalcate.visible				=	0
	dw_1.Object.totalprod.visible				=	1
	
ELSEIF ai_informe = 3 THEN
	dw_1.Object.totalfrigo.visible			=	0
	dw_1.Object.totalcama.visible				=	0
	dw_1.Object.totalband.visible				=	0
	dw_1.Object.totalespe.visible				=	1
	dw_1.Object.totalvari.visible				=	1
	dw_1.Object.totalgrup.visible				=	0
	dw_1.Object.totalsubg.visible				=	0
	dw_1.Object.totaltrat.visible				=	0
	dw_1.Object.totalperi.visible				=	1
	dw_1.Object.totalcate.visible				=	0
	dw_1.Object.totalprod.visible				=	0
	
ELSEIF ai_informe = 4 THEN
	dw_1.Object.totalfrigo.visible			=	1
	dw_1.Object.totalcama.visible				=	1
	dw_1.Object.totalband.visible				=	1
	dw_1.Object.totalespe.visible				=	1
	dw_1.Object.totalvari.visible				=	1
	dw_1.Object.totalgrup.visible				=	1
	dw_1.Object.totalsubg.visible				=	1
	dw_1.Object.totaltrat.visible				=	1
	dw_1.Object.totalperi.visible				=	1
	dw_1.Object.totalcate.visible				=	1
	dw_1.Object.totalprod.visible				=	1
END IF
end subroutine

event open;call super::open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp
iuo_camarasfrigo		=	Create uo_camarasfrigo
iuo_especie				=	Create uo_especie
iuo_grupoespecie		=	Create uo_grupoespecie
iuo_subgrupoespecie	=	Create uo_subgrupoespecie
iuo_variedades			=	Create uo_variedades
iuo_tratamientofrio	=	Create uo_tratamientofrio
iuo_periodofrio			=	Create uo_periodofrio
iuo_categorias			=	Create uo_categorias
iuo_productores		=	Create uo_productores
iuo_zonas				=	Create uo_zonas	
iuo_Cuartel				=	Create uo_ProdCuarteles
iuo_ProdPredio			=	Create uo_ProdPredio

//Exportador
dw_1.GetChild("exportador", idwc_exporta)
idwc_exporta.SetTransObject(sqlca)
IF idwc_exporta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Exportadores")
	idwc_exporta.InsertRow(0)
END IF

//Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

//Frigorifico
dw_1.GetChild("frigorifico", idwc_frigorifico)
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
idwc_variedad.InsertRow(0)

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
IF idwc_productor.Retrieve(0) = 0 THEN
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
END IF

dw_1.GetChild("predio", idwc_predio)
idwc_predio.SetTransObject(sqlca)

dw_1.GetChild("cuartel", idwc_Cuartel)
idwc_Cuartel.SetTransObject(sqlca)

dw_1.GetChild("mercado", idwc_certificacion)
idwc_certificacion.SetTransObject(sqlca)

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

dw_1.Object.cliente[1] = gi_codexport

dw_1.Object.fechainicio[1]	=	RelativeDate(Today(), -365)
dw_1.Object.fechatermi[1]	=	Today()

habilitatotal(1)
end event

on w_info_existencia_fgranel_camara_calidad.create
int iCurrent
call super::create
this.rb_inf_a=create rb_inf_a
this.rb_inf_b=create rb_inf_b
this.rb_inf_c=create rb_inf_c
this.rb_inf_d=create rb_inf_d
this.dw_1=create dw_1
this.dw_2=create dw_2
this.gb_3=create gb_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_inf_a
this.Control[iCurrent+2]=this.rb_inf_b
this.Control[iCurrent+3]=this.rb_inf_c
this.Control[iCurrent+4]=this.rb_inf_d
this.Control[iCurrent+5]=this.dw_1
this.Control[iCurrent+6]=this.dw_2
this.Control[iCurrent+7]=this.gb_3
end on

on w_info_existencia_fgranel_camara_calidad.destroy
call super::destroy
destroy(this.rb_inf_a)
destroy(this.rb_inf_b)
destroy(this.rb_inf_c)
destroy(this.rb_inf_d)
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.gb_3)
end on

type pb_excel from w_para_informes`pb_excel within w_info_existencia_fgranel_camara_calidad
integer x = 4978
integer y = 360
end type

type st_computador from w_para_informes`st_computador within w_info_existencia_fgranel_camara_calidad
end type

type st_usuario from w_para_informes`st_usuario within w_info_existencia_fgranel_camara_calidad
end type

type st_temporada from w_para_informes`st_temporada within w_info_existencia_fgranel_camara_calidad
end type

type p_logo from w_para_informes`p_logo within w_info_existencia_fgranel_camara_calidad
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_existencia_fgranel_camara_calidad
integer width = 4535
integer height = 76
integer textsize = -9
string text = "Informe Existencia Fruta Granel por Camara"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existencia_fgranel_camara_calidad
integer x = 4914
integer y = 688
integer taborder = 320
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	li_Frigorifico, li_Banda, li_Especie, li_Grupo, li_SubGrupo, &
			li_Variedad, li_Periodo, li_Categoria, li_ConsLote, li_exporta, li_planta,&
			li_condmerc, li_cuartel, li_predio, li_zona, li_kilosreales, li_condembq
			
Long		ll_Fila, ll_Camara, ll_Productor, ll_frigo
String	ls_Tratamiento, ls_titulo
Date		ld_fecini, ld_fecter

datawindowchild ldwc_planta

istr_info.titulo	= 'EXISTENCIA DE FRUTA GRANEL POR CAMARA'

OpenWithParm(vinf, istr_info)

IF rb_inf_a.checked THEN
	vinf.dw_1.DataObject = "dw_info_existencia_camara_a"
	ls_titulo				=	rb_inf_a.Text
ELSEIF rb_inf_b.checked THEN
	vinf.dw_1.DataObject = "dw_info_existencia_camara_b"
	ls_titulo				=	rb_inf_b.Text
ELSEIF rb_inf_c.checked THEN
	vinf.dw_1.DataObject = "dw_info_existencia_camara_c"
	ls_titulo				=	rb_inf_c.Text
ELSEIF rb_inf_d.checked THEN
	vinf.dw_1.DataObject = "dw_info_existencia_camara"
	ls_titulo				=	rb_inf_d.Text
END IF

dw_1.accepttext()

//Acepta Exportador
li_exporta = dw_1.Object.cliente[1]
IF IsNull(li_exporta) Then
	MessageBox( "Exportador Erróneo", "Falta seleccionar un Exportador.", &
				 StopSign!, Ok!)
	RETURN 1				 
END IF

li_kilosreales	=	dw_1.Object.kilosreales[1]
// Acepta Planta //
IF dw_1.Object.todosplanta[1] = 1 THEN
	li_Planta = 10000
ELSE
	li_Planta = dw_1.Object.planta[1]
	IF IsNull(li_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END IF
END IF

// Acepta Frigorifico //
IF dw_1.Object.TodosFrigo[1] = 1 THEN
	ll_Frigo = 10000
	IF dw_1.Object.consfrigo[1] = 1 THEN
		ll_Frigo = 90000
	END IF
ELSE
	ll_Frigo	= dw_1.Object.frigorifico[1]
	IF IsNull(ll_Frigo) THEN
		MessageBox( "Frigorifico Erróneo", "Falta seleccionar un Frigorifico.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF	
	
// Acepta Camara 
IF dw_1.Object.todoscama[1] = 1 THEN
	ll_Camara = 10000
	IF dw_1.Object.conscama[1] =1 THEN	
		ll_Camara	=	90000
	END IF
ELSE
	ll_Camara	=	dw_1.Object.camara[1]
	If IsNull(ll_Camara) Then
		MessageBox( "Camara Errónea", "Falta seleccionar una Camara.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

IF dw_1.Object.todasbanda[1]=1 THEN
	li_banda = 100
	IF dw_1.Object.consbanda[1]=1 THEN
		li_Banda  =   900
	END IF
ELSE
	li_Banda = dw_1.Object.banda[1]
	If IsNull(li_Banda) Then
		MessageBox( "Banda Errónea", "Falta seleccionar una Banda.", &
	             StopSign!, Ok!)
		RETURN 1				 
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
		RETURN 1				 
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
		RETURN 1				 
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
			RETURN 1				 
   	END IF
	END IF
ELSE
	li_Grupo = 100
End IF

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
				RETURN 1				 
   		END If
		END IF
	ELSE
		li_SubGrupo = 100
End IF

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
		RETURN 1				 
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
		RETURN 1				 
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
		RETURN 1				 
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
		RETURN 1				 
   END If
END IF

// Acepta Todos Lotes o Consolida
IF dw_1.Object.consollote[1] = 1 THEN
		li_ConsLote =	1
	ELSE
		li_ConsLote = 0
END IF

// Acepta Zona
IF dw_1.Object.todosZona[1] = 1 THEN
	li_Zona = -1
ELSE
	li_Zona = dw_1.Object.Zona[1]
	If IsNull(li_Zona) Then
		MessageBox( "Zona Errónea", "Falta seleccionar una Zona.", &
	             StopSign!, Ok!)
		RETURN				 
   END If
END IF

// Acepta Predio
IF dw_1.Object.todospredio[1] = 1 THEN
	li_Predio = -1
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

//Acepta condicion de embarque
IF dw_1.Object.todosembq[1] = 1 THEN
	li_condembq = -1
ELSE
	li_condembq  = dw_1.Object.embq[1]
	If IsNull(li_condembq ) Then
		MessageBox( "Embarque Erróneo", "Falta seleccionar un Embarque.", &
	             StopSign!, Ok!)
		RETURN				 
   END If
END IF

vinf.dw_1.getchild('plde_codigo',ldwc_planta)
ldwc_planta.settransobject(sqlca)
ldwc_planta.retrieve(gi_codexport)

ld_fecini	=	dw_1.Object.FechaInicio[1]
ld_fecter	=	dw_1.Object.FechaTermi[1]

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_planta,ll_frigo,ll_Camara,li_banda,li_Especie,li_Grupo,&
										 li_SubGrupo,li_Variedad,ls_Tratamiento,li_Periodo,li_Categoria,&
										 ll_Productor,li_ConsLote, li_exporta,&
										 li_Predio, li_Cuartel, li_condmerc,li_zona,li_kilosreales,&
										 ld_fecini, ld_fecter, li_condembq)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	F_Membrete(vinf.dw_1)

	IF rb_inf_a.checked THEN
		IF dw_1.Object.totalespe[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.2.Height=0")
		IF dw_1.Object.totalfrigo[1]= 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.3.Height=0")	
		IF dw_1.Object.totalcama[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.4.Height=0")	
		IF dw_1.Object.totalperi[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.5.Height=0")
		IF dw_1.Object.totalprod[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.6.Height=0")
		IF dw_1.Object.totalvari[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.7.Height=0")
	ELSEIF rb_inf_b.checked THEN
		IF dw_1.Object.totalespe[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.2.Height=0")
		IF dw_1.Object.totalfrigo[1]= 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.3.Height=0")	
		IF dw_1.Object.totalcama[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.4.Height=0")	
		IF dw_1.Object.totalperi[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.5.Height=0")
		IF dw_1.Object.totalvari[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.6.Height=0")
		IF dw_1.Object.totalprod[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.8.Height=0")	
	ELSEIF rb_inf_c.checked THEN
		IF dw_1.Object.totalespe[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.2.Height=0")
		IF dw_1.Object.totalperi[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.3.Height=0")	
		IF dw_1.Object.totalvari[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.4.Height=0")	
		IF dw_1.Object.totalfrigo[1]= 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.6.Height=0")
		IF dw_1.Object.totalcama[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.7.Height=0")
	ELSEIF rb_inf_d.checked THEN
		IF dw_1.Object.totalfrigo[1]= 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.1.Height=0")
		IF dw_1.Object.totalcama[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.2.Height=0")	
		IF dw_1.Object.totalband[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.3.Height=0")	
		IF dw_1.Object.totalespe[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.4.Height=0")
		IF dw_1.Object.totalgrup[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.5.Height=0")
		IF dw_1.Object.totalsubg[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.6.Height=0")
		IF dw_1.Object.totalvari[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.7.Height=0")		
		IF dw_1.Object.totalcate[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.8.Height=0")		
		IF dw_1.Object.totaltrat[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.9.Height=0")		
		IF dw_1.Object.totalperi[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.10.Height=0")		
		IF dw_1.Object.totalprod[1] = 0 THEN vinf.dw_1.Modify("DataWindow.Trailer.11.Height=0")				
	END IF
	IF gs_Ambiente <> 'Windows' THEN
	   	F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
  	END IF
END IF

SetPointer(Arrow!)
 
end event

type pb_salir from w_para_informes`pb_salir within w_info_existencia_fgranel_camara_calidad
integer x = 4914
integer y = 1052
integer taborder = 330
end type

type rb_inf_a from radiobutton within w_info_existencia_fgranel_camara_calidad
integer x = 375
integer y = 440
integer width = 1582
integer height = 76
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie/Frigorífico/Cámara/Período/Productor/Variedad/Lote"
boolean checked = true
end type

event clicked;habilitatotal(1)
end event

type rb_inf_b from radiobutton within w_info_existencia_fgranel_camara_calidad
integer x = 375
integer y = 516
integer width = 1582
integer height = 76
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie/Frigorífico/Cámara/Período/Variedad/Lote/Productor"
end type

event clicked;habilitatotal(2)
end event

type rb_inf_c from radiobutton within w_info_existencia_fgranel_camara_calidad
integer x = 2194
integer y = 444
integer width = 2203
integer height = 76
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie/Período/Variedad/Lote/Frigorífico/Cámara"
end type

event clicked;habilitatotal(3)
end event

type rb_inf_d from radiobutton within w_info_existencia_fgranel_camara_calidad
integer x = 2194
integer y = 516
integer width = 2505
integer height = 76
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Frigorífico/Cámara/Banda/Especie/Grupo/SubGrupo/Variedad/Categoria/Tipo Frío/Periodo/Productor"
end type

event clicked;habilitatotal(4)
end event

type dw_1 from datawindow within w_info_existencia_fgranel_camara_calidad
integer x = 261
integer y = 632
integer width = 4535
integer height = 1092
integer taborder = 10
string title = "none"
string dataobject = "dw_info_existencia_camarabanda_seleccion"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null, li_cliente
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
		ElSE
			dw_1.Object.todoscama.protect = 0
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

	CASE "frigorifico"
		IF Not iuo_Camarasfrigo.Existe(dw_1.Object.Planta[1], Integer(data), True, Sqlca) THEN
			This.SetItem(1, "frigorifico", li_Null )
			This.SetFocus()
			RETURN 1
		ElSE
			dw_1.Object.todoscama.protect = 0
		END IF		

		idwc_camaras.SetFilter("cama_codfri = " + data + " And cama_codfri <> cama_codigo") 
		idwc_camaras.Filter()

	CASE "camara"
		IF NOT iuo_CamarasFrigo.Existe(This.Object.planta[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "camara", li_Null )
			This.SetFocus()
			RETURN 1
		END IF

	CASE "especie"
		IF NOT iuo_especie.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "especie", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			idwc_grupo.Retrieve(Integer(data),0)
			This.Object.Grupo[row]		=	li_Null
			/**/
			idwc_subgrupo.Retrieve(Integer(data),0)
			This.Object.SubGrupo[row]	=	li_Null
			/**/
			idwc_variedad.Retrieve(Integer(data))
			This.Object.Variedad[row]	=	li_Null

		END IF

	CASE "variedad"
		
		IF NOT iuo_Variedades.Existe(This.Object.especie[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "variedad", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			This.Object.Grupo[row]			=	iuo_Variedades.Grupo
			This.Object.SubGrupo[row]		=	iuo_Variedades.SubGrupo
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

	CASE "productor"
		IF NOT iuo_Productores.Existe(Long(data),True,SqlCa) THEN
			This.SetItem(1, "productor", li_Null)
			This.SetFocus()
			RETURN 1
		
		ELSE
			This.Object.predio[row]			  =	li_Null
			idwc_Predio.Retrieve(Long(Data))
			idwc_Cuartel.Reset()
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
		This.Object.TodosEspe[Row]		=	0
		This.Object.TodosVari[Row]		=	0
		
		idwc_variedad.settransobject(sqlca)
		idwc_variedad.Retrieve(iuo_Cuartel.Especie)
		
		This.Object.Especie[Row]		=	iuo_Cuartel.Especie
		This.Object.variedad[Row]		=	iuo_Cuartel.Variedad
	END IF

	CASE "todosplanta"
		IF data='0' THEN
			dw_1.Object.todoscama.protect = 1
		ELSE
		IF data = '1' THEN This.Object.planta[row]		=	li_Null
		   dw_1.Object.frigorifico[row] = li_Null
			dw_1.Object.todosfrigo[row] = 1
			dw_1.Object.camara[row]	=	li_Null
			dw_1.Object.todoscama[row]	=	1
			dw_1.Object.banda[row]		=  li_null
			dw_1.Object.todasbanda[row]  =	1
		END IF

	CASE "todosfrigo"
		IF data='0' THEN
			dw_1.Object.todoscama.protect = 1
		ELSE
		IF data = '1' THEN This.Object.frigorifico[row]	=	li_Null
			dw_1.Object.camara[row]		=	li_Null
			dw_1.Object.todoscama[row] = 1	
			dw_1.Object.banda[row]		=  li_null
			dw_1.Object.todasbanda[row]  =	1
		End If

	CASE "todoscama"
		IF data='0' THEN 
			dw_1.Object.todoscama.protect = 1
		ELSE
		IF data = '1' THEN This.Object.camara[row]	= li_Null
			dw_1.Object.camara[row]	=	li_Null
			dw_1.Object.todoscama[row]	=	1
			dw_1.Object.banda[row]		=  li_null
			dw_1.Object.todasbanda[row]  =	1
		END IF
		
	CASE	"todosespe"
		IF	data = '1' THEN This.Object.especie[row]	=	li_Null
			dw_1.Object.variedad[row]	= li_Null
			dw_1.Object.todosvari[row]	=	1

	CASE	"todostrata"
		IF	data = '1' THEN This.Object.tratamiento[row]	=	String(li_Null)
	
	CASE	"todosperio"
		IF	data = '1' THEN This.Object.periodo[row]	=	li_Null
	
	CASE	"todoscate"
		IF	data = '1' THEN This.Object.categoria[row]	=	li_Null
			
	CASE	"todosprod"
		dw_1.Object.TodosZona[row]			=	1
		dw_1.Object.TodosPredio[row]		=	1	
		dw_1.Object.TodosCuartel[row]		=	1
		dw_1.Object.TodosMercado[row]		=	1
		This.Object.Predio[row]				=	li_Null
		This.Object.Zona[row]				=	li_Null
		This.Object.Cuartel[row]			=	li_Null
		This.Object.Mercado[row]			=	li_Null
		IF	data = '1' THEN This.Object.productor[row]		=	li_Null
		
	CASE "Todoszona"
		IF Data = '1' THEN This.Object.Zona[row]				=	li_Null
		
	CASE "Todoscuartel"
		IF Data = '1' THEN This.Object.Cuartel[row]			=	li_Null
		
	CASE "todosembq"
		IF Data = '1' THEN This.Object.embq[row]				=	li_Null
		
	CASE "Todosmercado"
		IF Data = '1' THEN This.Object.Mercado[row]			=	li_Null
		
	CASE "todospredio"
		IF Data = '1' THEN
			This.Object.Cuartel[row]			=	li_Null
			This.Object.Mercado[row]			=	li_Null
			dw_1.Object.TodosCuartel[row]		=	1
			dw_1.Object.TodosMercado[row]		=	1
		END IF
		
	CASE  "consprod"
		IF data = '1' THEN 
			This.Object.consollote[row] = 1
			This.Object.consollote.Protect = 1
		ELSE
			This.Object.consollote[row] = 0
			This.Object.consollote.Protect = 0			
		END IF
		
	CASE  "consfrigo"
		IF data = '1' THEN
			This.Object.conscama[row] = 1
			This.Object.consbanda[row] = 1
			This.Object.conscama.Protect = 1
			This.Object.consbanda.Protect = 1			
		ELSE
			This.Object.conscama[row] = 0
			This.Object.consbanda[row] = 0
			This.Object.conscama.Protect = 0
			This.Object.consbanda.Protect = 0			
		END IF
		
	CASE "conscama"
		IF data = '1' THEN
			dw_1.Object.consbanda[row] = 1
			This.Object.consbanda.Protect = 1			
		ELSE
			dw_1.Object.consbanda[row] = 0
			This.Object.consbanda.Protect = 0			
		END IF
		
	CASE "todosfecha"
		dw_1.Object.fechainicio[1]	=	RelativeDate(Today(), -365)
		dw_1.Object.fechatermi[1]	=	Today()
		
	CASE "fechainicio"
		IF Date(data) > dw_1.Object.fechatermi[1] THEN
			This.SetItem(1, "fechainicio", RelativeDate(Today(), -365))
			This.SetFocus()
			RETURN 1
		END IF
		
	CASE "fechatermino"
		IF Date(data) < dw_1.Object.fechainicio[1] THEN
			This.SetItem(1, "fechatermino", Today())
			This.SetFocus()
			RETURN 1
		END IF
	
END CHOOSE
end event

event itemerror;RETURN 1
end event

type dw_2 from datawindow within w_info_existencia_fgranel_camara_calidad
boolean visible = false
integer x = 4955
integer y = 1400
integer width = 151
integer height = 132
integer taborder = 340
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_existencia_camara_c"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type gb_3 from groupbox within w_info_existencia_fgranel_camara_calidad
integer x = 261
integer y = 384
integer width = 4535
integer height = 244
integer taborder = 10
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
string text = "Ordenado Por"
end type

