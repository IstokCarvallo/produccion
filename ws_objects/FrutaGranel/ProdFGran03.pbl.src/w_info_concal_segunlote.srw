$PBExportHeader$w_info_concal_segunlote.srw
$PBExportComments$Ventana de Informe de Control de Calidad Recepción Distribución Calibres y Color Según el tipo de Lote
forward
global type w_info_concal_segunlote from w_para_informes
end type
type dw_1 from datawindow within w_info_concal_segunlote
end type
type rb_detfriprod from radiobutton within w_info_concal_segunlote
end type
type rb_detprod from radiobutton within w_info_concal_segunlote
end type
type rb_resprofri from radiobutton within w_info_concal_segunlote
end type
type rb_lotrecep from radiobutton within w_info_concal_segunlote
end type
type rb_lotproc from radiobutton within w_info_concal_segunlote
end type
type rb_lotexist from radiobutton within w_info_concal_segunlote
end type
type rb_existproce from radiobutton within w_info_concal_segunlote
end type
type rb_resproesp from radiobutton within w_info_concal_segunlote
end type
type gb_3 from groupbox within w_info_concal_segunlote
end type
type gb_4 from groupbox within w_info_concal_segunlote
end type
end forward

global type w_info_concal_segunlote from w_para_informes
integer x = 14
integer y = 32
integer width = 2656
integer height = 2036
string title = "Control de Calidad Recepción"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_1 dw_1
rb_detfriprod rb_detfriprod
rb_detprod rb_detprod
rb_resprofri rb_resprofri
rb_lotrecep rb_lotrecep
rb_lotproc rb_lotproc
rb_lotexist rb_lotexist
rb_existproce rb_existproce
rb_resproesp rb_resproesp
gb_3 gb_3
gb_4 gb_4
end type
global w_info_concal_segunlote w_info_concal_segunlote

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
uo_predios           iuo_predios
uo_centrocostos      iuo_centrocosto

DataWindowChild	idwc_planta, idwc_frigorifico, idwc_categorias, idwc_camaras, idwc_especie, &
						idwc_variedad, idwc_tratamiento, idwc_periodo, &
						idwc_productor, idwc_predio, idwc_ccosto, idwc_exportadores

String	is_NomPlanta
Integer	ii_Planta, ii_Frigorifico, ii_Especie, ii_Grupo, ii_productor, ii_predio
end variables

forward prototypes
public function boolean noexistetipoenvase (integer ai_tipo_enva)
end prototypes

public function boolean noexistetipoenvase (integer ai_tipo_enva);String ls_tipo

SELECT	tien_nombre
	INTO	:ls_tipo
	FROM	dba.tiposenvases
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

on w_info_concal_segunlote.create
int iCurrent
call super::create
this.dw_1=create dw_1
this.rb_detfriprod=create rb_detfriprod
this.rb_detprod=create rb_detprod
this.rb_resprofri=create rb_resprofri
this.rb_lotrecep=create rb_lotrecep
this.rb_lotproc=create rb_lotproc
this.rb_lotexist=create rb_lotexist
this.rb_existproce=create rb_existproce
this.rb_resproesp=create rb_resproesp
this.gb_3=create gb_3
this.gb_4=create gb_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
this.Control[iCurrent+2]=this.rb_detfriprod
this.Control[iCurrent+3]=this.rb_detprod
this.Control[iCurrent+4]=this.rb_resprofri
this.Control[iCurrent+5]=this.rb_lotrecep
this.Control[iCurrent+6]=this.rb_lotproc
this.Control[iCurrent+7]=this.rb_lotexist
this.Control[iCurrent+8]=this.rb_existproce
this.Control[iCurrent+9]=this.rb_resproesp
this.Control[iCurrent+10]=this.gb_3
this.Control[iCurrent+11]=this.gb_4
end on

on w_info_concal_segunlote.destroy
call super::destroy
destroy(this.dw_1)
destroy(this.rb_detfriprod)
destroy(this.rb_detprod)
destroy(this.rb_resprofri)
destroy(this.rb_lotrecep)
destroy(this.rb_lotproc)
destroy(this.rb_lotexist)
destroy(this.rb_existproce)
destroy(this.rb_resproesp)
destroy(this.gb_3)
destroy(this.gb_4)
end on

event open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp
iuo_camarasfrigo		=	Create uo_camarasfrigo
iuo_especie				=	Create uo_especie
iuo_variedades			=	Create uo_variedades
iuo_tratamientofrio	=	Create uo_tratamientofrio
iuo_periodofrio		=	Create uo_periodofrio
iuo_categorias			=	Create uo_categorias
iuo_productores		=	Create uo_productores
iuo_predios				=	Create uo_predios
iuo_centrocosto		=	Create uo_centrocostos

//Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

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
IF idwc_productor.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
END IF

//Predio
dw_1.GetChild("predio",idwc_predio)
idwc_predio.SetTransObject(Sqlca)
IF idwc_predio.Retrieve(0) = 0 THEN
	//MessageBox("Atención","Falta Registrar Predios")
	idwc_predio.InsertRow(0)
END IF

//Centro Costo
dw_1.GetChild("centrocosto",idwc_ccosto)
idwc_ccosto.SetTransObject(Sqlca)
IF idwc_ccosto.Retrieve(0,0) = 0 THEN
	//MessageBox("Atención","Falta Registrar Centros de Costo")
	idwc_ccosto.InsertRow(0)
END IF

//exportador
dw_1.GetChild("exportador",idwc_exportadores)
idwc_exportadores.SetTransObject(Sqlca)
idwc_exportadores.Retrieve() 


dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

Paramtemporada(gstr_paramtempo)
dw_1.SetItem(1,"fechad", gstr_paramtempo.fechainicio)
dw_1.SetItem(1,"fechah", Today())
dw_1.Setitem(1,"exportador",gi_codexport)
end event

type st_titulo from w_para_informes`st_titulo within w_info_concal_segunlote
string tag = "Informe Control de Calidad"
integer x = 133
integer y = 60
integer width = 1888
string text = "Informe Control de Calidad "
boolean righttoleft = true
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_concal_segunlote
integer x = 2245
integer y = 268
integer height = 140
integer taborder = 40
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer  il_tiplote, il_tipinfo
long	   ll_Planta, ll_Especie, ll_Variedad, ll_Periodo,  ll_Categoria, &
			ll_tipoenvase, ll_Fila, ll_camara, ll_productor, ll_Exportador
Date		ld_Fechadesde,   ld_FechaHasta, ld_fechaprueba
String	ls_Tratamiento,   ls_fecha


istr_info.titulo	= 'CONTROL DE CALIDAD '

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_concal_distrib_calibres"
dw_1.accepttext()

// Acepta Exportador //
ll_Exportador = dw_1.Object.exportador[1]
IF IsNull(ll_Exportador) Then
	MessageBox( "Cliente Erróneo", "Falta seleccionar un Cliente.", &
				 StopSign!, Ok!)
	RETURN 1				 
END IF


// Acepta Planta //
IF dw_1.Object.todosplanta[1] = 1 THEN
	ll_Planta = 0
ELSE
	ll_Planta = dw_1.Object.planta[1]
	IF IsNull(ll_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END IF
END IF

// Acepta Camara //
IF dw_1.Object.todascamara[1] = 1 THEN
	ll_Camara = 0
ELSE
	ll_Camara	=	dw_1.Object.camara[1]
	If IsNull(ll_Camara) Then
		MessageBox( "Camara Errónea", "Falta seleccionar una Camara.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Especie //
IF dw_1.Object.todosespe[1] = 1 THEN
	ll_Especie = 0
ELSE
	ll_Especie = dw_1.Object.especie[1]
	If IsNull(ll_especie) Then
		MessageBox( "Especie Errónea", "Falta seleccionar una Especie.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Variedad //
IF dw_1.Object.todosvari[1] = 1 THEN
	ll_Variedad = 0
ELSE
	ll_Variedad = dw_1.Object.variedad[1]
	If IsNull(ll_Variedad) Then
		MessageBox( "Variedad Errónea", "Falta seleccionar una Variedad.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Tratamientos de frio //
IF dw_1.Object.todostrata[1] = 1 THEN
	ls_Tratamiento = 'Z'
ELSE
	ls_Tratamiento = dw_1.Object.tratamiento[1]
	If IsNull(ls_Tratamiento) or ls_tratamiento="" Then
		MessageBox( "Tratamiento Erróneo", "Falta seleccionar un Tratamiento.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

// Acepta Productor
IF dw_1.Object.todosprod[1] = 1 THEN
	ll_Productor = 0
ELSE
	ll_Productor = dw_1.Object.productor[1]
	If IsNull(ll_Productor) Then
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.", &
	             StopSign!, Ok!)
		RETURN 1				 
   END If
END IF

ld_fechadesde = dw_1.Object.fechad[1]

If IsNull(ld_fechadesde) or ld_fechadesde<= ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN 1				 
END If

ls_fecha=mid(String(dw_1.Object.fechah[1],'dd/mm/yyyy'),1,10)
ld_fechahasta = Date(ls_fecha)

If IsNull(ld_fechahasta) or ld_fechahasta<=ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", &
	             StopSign!, Ok!)
	RETURN 1				 
END If

IF ld_fechadesde>=ld_fechahasta THEN
	MessageBox( "Fechas Erroneas", "La Fecha Hasta debe ser mayor a la Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN 1
END IF	

// Tipo de Lote
IF rb_lotrecep.Checked THEN
	il_Tiplote = 1
ELSEIF rb_lotproc.Checked THEN
		 il_Tiplote = 2
	 ELSE
		 il_Tiplote = 3
END IF

// Tipo de Informe
IF rb_detfriprod.Checked THEN
	il_Tipinfo = 1
ELSEIF rb_detprod.checked THEN
	    il_Tipinfo = 2
		 ELSEIF rb_resprofri.Checked THEN
				  il_Tipinfo = 3
		     ELSEIF rb_resproesp.Checked THEN
		        		il_Tipinfo = 4
			      ELSE
						il_TipInfo = 5

END IF

vinf.dw_1.SetTransObject(sqlca)

	
ll_Fila	=	vinf.dw_1.Retrieve(ll_exportador, ll_Planta,ll_Camara, ll_Especie, ll_Variedad,  &
										 ll_Productor, ls_Tratamiento, ld_fechadesde, ld_fechahasta, &
										 il_Tiplote, il_Tipinfo)

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
	
	IF il_TipInfo = 1 THEN vinf.dw_1.Modify("Datawindow.Detail.Height=0")		

	IF il_TipInfo = 2 THEN vinf.dw_1.Modify("Datawindow.Header.2.Height=0")		
	IF il_TipInfo = 2 THEN vinf.dw_1.Modify("Datawindow.Trailer.2.Height=0")		
	
	IF il_TipInfo = 3 THEN vinf.dw_1.Modify("Datawindow.Detail.Height=0")

	IF il_TipInfo = 4 THEN vinf.dw_1.Modify("Datawindow.Header.2.Height=0")
	IF il_TipInfo = 4 THEN vinf.dw_1.Modify("DataWindow.Detail.Height=0")
	IF il_TipInfo = 4 THEN vinf.dw_1.Modify("DataWindow.Trailer.2.Height=0")

	IF il_TipInfo = 5 THEN vinf.dw_1.Modify("Datawindow.Header.2.Height=0")
	IF il_TipInfo = 5 THEN vinf.dw_1.Modify("DataWindow.Detail.Height=0")
	IF il_TipInfo = 5 THEN vinf.dw_1.Modify("DataWindow.Trailer.4.Height=0")
	IF il_TipInfo = 5 THEN vinf.dw_1.Modify("DataWindow.Trailer.2.Height=0")
	IF il_TipInfo = 5 THEN vinf.dw_1.Modify("DataWindow.Trailer.1.Height=0")	


	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)


end event

type pb_salir from w_para_informes`pb_salir within w_info_concal_segunlote
integer x = 2245
integer y = 556
integer height = 140
integer taborder = 50
end type

type dw_1 from datawindow within w_info_concal_segunlote
integer x = 137
integer y = 880
integer width = 1888
integer height = 1020
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_ctrlcal_seleccion"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null,li_cliente
String	ls_Columna

SetNull(li_Null)
dw_1.accepttext()

li_cliente =  dw_1.Object.exportador[1]

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "planta"
		IF NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) THEN
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			RETURN 1
			dw_1.Object.todascamara.protect = 0
		END IF		
		
		IF idwc_camaras.Retrieve(Integer(data)) = 0 THEN
			MessageBox("Atención","Falta Registrar Cámaras Frigoríficas.")
			RETURN 1
		END IF
						
	CASE "camara"
		IF NOT iuo_CamarasFrigo.Existe(This.Object.planta[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "camara", li_Null )
			This.SetFocus()
			RETURN 1
		Else
			dw_1.Object.todascamara.protect = 0
		END IF

	CASE "especie"
		
		IF NOT iuo_Especie.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "especie", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			idwc_variedad.Retrieve(Integer(data))
			This.Object.Variedad[row]	=	li_Null
		END IF

	CASE "variedad"
		
		IF NOT iuo_Variedades.Existe(This.Object.especie[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "variedad", li_Null )
			This.SetFocus()
			RETURN 1
		END IF
		
	CASE "tratamiento"
		IF NOT iuo_TratamientoFrio.Ofp_Recupera_TratamientoFrio(SqlCa,data,True) THEN
			This.SetItem(1, "tratamiento", String(li_Null))
			This.SetFocus()
			RETURN 1
		END IF

	CASE "productor"
		IF NOT iuo_Productores.Existe(Long(data),True,SqlCa) THEN
			This.SetItem(1, "productor", li_Null)
			This.SetFocus()
			RETURN 1
		END IF

	CASE "todosplanta"
		IF data='0' THEN 	dw_1.Object.todascamara.protect = 0

		IF data = '1' THEN 
			This.Object.planta[row]		=	li_Null
			dw_1.Object.camara[row]		=	li_Null
			dw_1.Object.todascamara[row]	=	1
		END IF

	CASE "todascamara"
		IF data='0' THEN 
			dw_1.Object.todascamara.protect = 1
		ELSE
			IF data = '1' THEN dw_1.Object.camara[row]	=	li_Null
			dw_1.Object.todascamara[row]	=	1
		END IF
		
	CASE	"todosespe"
		IF	data = '1' THEN This.Object.especie[row]	=	li_Null
			dw_1.Object.variedad[row]	= li_Null
			dw_1.Object.todosvari[row]	=	1

	CASE	"todostrata"
		IF	data = '1' THEN This.Object.tratamiento[row]	=	String(li_Null)
	
			
	CASE	"todosprod"
		IF	data = '1' THEN This.Object.productor[row]	=	li_Null

Return 0
END CHOOSE
end event

type rb_detfriprod from radiobutton within w_info_concal_segunlote
integer x = 288
integer y = 544
integer width = 777
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Detalle Frigo/Productor"
boolean checked = true
end type

type rb_detprod from radiobutton within w_info_concal_segunlote
integer x = 288
integer y = 652
integer width = 654
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Detalle Productor"
end type

type rb_resprofri from radiobutton within w_info_concal_segunlote
integer x = 1097
integer y = 544
integer width = 818
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Res. Productor por Frigo"
end type

type rb_lotrecep from radiobutton within w_info_concal_segunlote
integer x = 279
integer y = 284
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
long backcolor = 12632256
string text = "Recepción"
boolean checked = true
end type

event clicked;rb_existproce.visible = False
end event

type rb_lotproc from radiobutton within w_info_concal_segunlote
integer x = 878
integer y = 284
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
long backcolor = 12632256
string text = "Procesado"
end type

event clicked;rb_existproce.visible = True
end event

type rb_lotexist from radiobutton within w_info_concal_segunlote
integer x = 1431
integer y = 284
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
long backcolor = 12632256
string text = "Existencia"
end type

event clicked;rb_existproce.visible = True
end event

type rb_existproce from radiobutton within w_info_concal_segunlote
boolean visible = false
integer x = 288
integer y = 768
integer width = 635
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Resumen Variedad"
end type

type rb_resproesp from radiobutton within w_info_concal_segunlote
integer x = 1097
integer y = 652
integer width = 878
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Res. Productor por Especie"
end type

type gb_3 from groupbox within w_info_concal_segunlote
integer x = 137
integer y = 452
integer width = 1883
integer height = 412
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Tipo de Informe"
end type

type gb_4 from groupbox within w_info_concal_segunlote
integer x = 137
integer y = 168
integer width = 1883
integer height = 264
integer taborder = 10
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Tipo de Lote"
end type

