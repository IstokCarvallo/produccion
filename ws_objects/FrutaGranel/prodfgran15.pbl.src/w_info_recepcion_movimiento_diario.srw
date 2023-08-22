$PBExportHeader$w_info_recepcion_movimiento_diario.srw
$PBExportComments$Informe Cartola de Movimientos Fruta Granel
forward
global type w_info_recepcion_movimiento_diario from w_para_informes
end type
type dw_1 from datawindow within w_info_recepcion_movimiento_diario
end type
type rb_1 from radiobutton within w_info_recepcion_movimiento_diario
end type
type rb_2 from radiobutton within w_info_recepcion_movimiento_diario
end type
end forward

global type w_info_recepcion_movimiento_diario from w_para_informes
string tag = "Informe Recepciones Fruta Granel"
integer x = 14
integer y = 32
integer width = 3045
integer height = 2136
string title = "Informe Recepciones Fruta Granel"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_1 dw_1
rb_1 rb_1
rb_2 rb_2
end type
global w_info_recepcion_movimiento_diario w_info_recepcion_movimiento_diario

type variables
uo_plantadesp			iuo_plantadesp
uo_camarasfrigo		iuo_camarasfrigo
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
uo_ProdCuarteles			iuo_Cuartel
uo_ProdPredio			iuo_ProdPredio
uo_tipomovtofruta		iuo_tipomovtofruta
uo_Clientesprod			iuo_Cliente

DataWindowChild	idwc_planta, idwc_frigorifico, idwc_categorias, idwc_camaras, idwc_especie, &
						idwc_grupo, idwc_subgrupo, idwc_variedad, idwc_tratamiento, idwc_periodo, &
						idwc_productor, idwc_ccosto, idwc_tipoenvase, idwc_sentido, &
						idwc_movto, idwc_exporta, idwc_predio, idwc_Cuartel, idwc_certificacion

String	is_NomPlanta
Integer	ii_Planta, ii_Frigorifico, ii_Especie, ii_Grupo, ii_productor, ii_predio, ii_recepciones, ii_sentidomov
end variables

on w_info_recepcion_movimiento_diario.create
int iCurrent
call super::create
this.dw_1=create dw_1
this.rb_1=create rb_1
this.rb_2=create rb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
this.Control[iCurrent+2]=this.rb_1
this.Control[iCurrent+3]=this.rb_2
end on

on w_info_recepcion_movimiento_diario.destroy
call super::destroy
destroy(this.dw_1)
destroy(this.rb_1)
destroy(this.rb_2)
end on

event open;call super::open;x	=	0
y	=	0

iuo_plantadesp			=	Create	uo_plantadesp
iuo_especie				=	Create	uo_especie
iuo_grupoespecie		=	Create	uo_grupoespecie
iuo_subgrupoespecie	=	Create 	uo_subgrupoespecie
iuo_variedades			=	Create 	uo_variedades
iuo_tratamientofrio	=	Create 	uo_tratamientofrio
iuo_periodofrio			=	Create 	uo_periodofrio
iuo_categorias			=	Create 	uo_categorias
iuo_productores		=	Create 	uo_productores
iuo_predios				=	Create 	uo_predios
iuo_centrocosto		=	Create 	uo_centrocostos
iuo_camarasfrigo		=  Create 	uo_camarasfrigo	
iuo_zonas				=	Create 	uo_zonas	
iuo_Cuartel				=	Create 	uo_ProdCuarteles
iuo_ProdPredio			=	Create 	uo_ProdPredio
iuo_tipomovtofruta		=	Create	uo_tipomovtofruta
iuo_Cliente				=	Create	uo_Clientesprod

ii_sentidomov 			=	Integer(Message.StringParm)

rb_1.Checked = True
rb_1.TriggerEvent("Clicked")

//Exportador
dw_1.GetChild("cliente", idwc_exporta)
idwc_exporta.SetTransObject(sqlca)
IF idwc_exporta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Exportador")
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
IF idwc_variedad.Retrieve(-1) = 0 THEN
	//MessageBox("Atención","Falta Registrar Variedades")
	idwc_variedad.InsertRow(0)
END IF

//Tratamiento
dw_1.GetChild("tratamiento",idwc_tratamiento)
idwc_tratamiento.SetTransObject(Sqlca)
IF idwc_tratamiento.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Tratamientos")
	idwc_tratamiento.InsertRow(0)
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

IF ii_sentidomov = 1 THEN
	st_titulo.text 	= 	"Informe Recepción de Fruta Granel Diaria"
	This.Title			=	"Informe Recepciones Fruta Granel"
	rb_2.Visible		=	True
ELSE
	st_titulo.text 	= 	"Informe Despacho de Fruta Granel Diaria"
	This.Title			=	"Informe Despachos Fruta Granel"
	rb_2.Visible		=	False
	ii_sentidomov		=	2
END IF

//Movimiento
dw_1.GetChild("Movimiento", idwc_movto)
idwc_movto.SetTransObject(sqlca)
IF idwc_movto.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Movimientos")
	idwc_Movto.InsertRow(0)
ELSE
	idwc_movto.SetFilter("tpmv_tipcor = " + String(ii_sentidomov) + " and tpmv_frugra = 1")
	idwc_movto.Filter()
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
dw_1.Setitem(1,"cliente",gi_codexport)

end event

type pb_excel from w_para_informes`pb_excel within w_info_recepcion_movimiento_diario
integer x = 2661
integer y = 328
end type

type st_computador from w_para_informes`st_computador within w_info_recepcion_movimiento_diario
end type

type st_usuario from w_para_informes`st_usuario within w_info_recepcion_movimiento_diario
end type

type st_temporada from w_para_informes`st_temporada within w_info_recepcion_movimiento_diario
end type

type p_logo from w_para_informes`p_logo within w_info_recepcion_movimiento_diario
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_recepcion_movimiento_diario
integer x = 247
integer width = 2235
string text = "Informe Recepción de Fruta Granel Diaria"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_recepcion_movimiento_diario
integer x = 2629
integer y = 684
integer taborder = 370
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	li_Planta, li_Especie, li_Grupo, li_SubGrupo, li_Ccosto, &
			li_Variedad, li_Periodo, li_Categoria, li_tipoenvase, &
			li_ConsLote, li_Consfechad, li_sentidomov, li_movto, li_exporta = -1,&
			li_condmerc, li_cuartel, li_predio, li_zona, li_consfeccos
			
Long		ll_Fila, ll_Productor, ll_Frigo, ll_Camara
Date		ld_Fechadesde, ld_FechaHasta, ld_fechaprueba
String	ls_Tratamiento, ls_fecha, ls_titulo

istr_info.titulo	= 'RECEPCIONES DIARIAS FRUTA GRANEL'

OpenWithParm(vinf, istr_info)

If rb_1.Checked Then
	If ii_sentidomov = 1 Then
		vinf.dw_1.DataObject = 	"dw_info_recepcion_frutagranel_diaria"
		ls_titulo					=	rb_1.Text
	Else
		vinf.dw_1.DataObject = 	"dw_info_despacho_frutagranel_diaria"
		ls_titulo					=	rb_1.Text
	End If
Else
	vinf.dw_1.DataObject = 	"dw_info_recepcion_frutagranel_diaria_det"
	ls_titulo					=	rb_2.Text
End If

dw_1.accepttext()

//Acepta Exportador
If Not dw_1.Object.todos[1] = 1 Then li_exporta = dw_1.Object.cliente[1]

If IsNull(li_exporta) Then
	MessageBox( "Cliente Erróneo", "Falta Seleccionar un Cliente.", StopSign!, Ok!)
	Return 1				 
End If

// Acepta Planta //
If dw_1.Object.todosplanta[1] = 1 Then
	li_Planta = 10000
Else
	li_Planta = dw_1.Object.planta[1]
	If IsNull(li_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", StopSign!, Ok!)
		Return 1				 
   End If
End If


// Acepta FrigorIfico //
If dw_1.Object.TodosFrigo[1] = 1 Then
	ll_Frigo = 10000
	If dw_1.Object.consfrigo[1] = 1 Then
		ll_Frigo = 90000
	End If
Else
	ll_Frigo	= dw_1.Object.frigo[1]
	If IsNull(ll_Frigo) Then
		MessageBox( "FrigorIfico Erróneo", "Falta seleccionar un FrigorIfico.", StopSign!, Ok!)
		Return 1				 
   End If
End If	

// Acepta Camara //
If dw_1.Object.todoscamara[1] = 1 Then
	ll_Camara = 10000
	If dw_1.Object.conscamara[1] = 1 Then	
		ll_Camara	=	90000
	End If
Else
	ll_Camara	=	dw_1.Object.camara[1]
	If IsNull(ll_Camara) Then
		MessageBox( "Camara Errónea", "Falta seleccionar una Camara.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Especie //
If dw_1.Object.todosespe[1] = 1 Then
	li_Especie = 100
Else
	li_Especie = dw_1.Object.especie[1]
	If IsNull(li_especie) Then
		MessageBox( "Especie Errónea", "Falta seleccionar una Especie.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Variedad //
If dw_1.Object.todosvari[1] = 1 Then
	li_Variedad = 10000
Else
	li_Variedad = dw_1.Object.variedad[1]
	If IsNull(li_Variedad) Then
		MessageBox( "Variedad Errónea", "Falta seleccionar una Variedad.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Tratamientos de frio //
If dw_1.Object.todostrata[1] = 1 Then
	ls_Tratamiento = '*'
	If dw_1.Object.constrata[1] = 1 Then	
		ls_Tratamiento = '**'
	End If
Else
	ls_Tratamiento = dw_1.Object.tratamiento[1]
	If IsNull(ls_Tratamiento) or ls_tratamiento="" Then
		MessageBox( "Tratamiento Erróneo", "Falta seleccionar un Tratamiento.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Categorias
If dw_1.Object.todoscate[1] = 1  Then
	li_Categoria = 1000
Else
	li_Categoria = dw_1.Object.categoria[1]
	If IsNull(li_Categoria) Then
		MessageBox( "Categoria Errónea", "Falta seleccionar una Categoria.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Productor
If dw_1.Object.todosprod[1] = 1 Then
	ll_Productor = 10000
	If dw_1.Object.consprod[1] = 1 Then	
		ll_Productor =	90000
	End If
Else
	ll_Productor = dw_1.Object.productor[1]
	If IsNull(ll_Productor) Then
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.", StopSign!, Ok!)
		Return 1				 
   End If
End If

ld_fechadesde = dw_1.Object.fechad[1]

If IsNull(ld_fechadesde) or ld_fechadesde<= ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", StopSign!, Ok!)
	Return 1				 
End If

ls_fecha			=	String(dw_1.Object.fechah[1])
ld_fechahasta = 	Date(ls_fecha)
If IsNull(ld_fechahasta) or ld_fechahasta<=ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", StopSign!, Ok!)
	Return 1				 
End If

If ld_fechadesde > ld_fechahasta Then
	MessageBox( "Fechas Erroneas", "La Fecha Hasta debe ser mayor o igual a la Fecha Desde.", StopSign!, Ok!)
	Return 1
End If	

//li_sentidomov = 1

// Acepta Movimiento //
If dw_1.Object.todosmovto[1]=1 Then
	li_movto = 1000
	If dw_1.Object.consmovto[1]=1 Then	
		li_movto =	9000
	End If
Else
	li_movto = dw_1.Object.movimiento[1]
	If IsNull(li_movto) Then
		MessageBox( "Movimiento Erróneo", "Falta seleccionar un Movimiento.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Zona
If dw_1.Object.todosZona[1] = 1 Then
	li_Zona = -1
Else
	li_Zona = dw_1.Object.Zona[1]
	If IsNull(li_Zona) Then
		MessageBox( "Zona Errónea", "Falta seleccionar una Zona.", StopSign!, Ok!)
		Return				 
   End If
End If

// Acepta Predio
If dw_1.Object.todospredio[1] = 1 Then
	li_Predio = -1
Else
	li_Predio = dw_1.Object.predio[1]
	If IsNull(li_Predio) Then
		MessageBox( "Predio Erróneo", "Falta seleccionar un Predio.", StopSign!, Ok!)
		Return				 
   End If
End If

//Acepta Cuartel
If dw_1.Object.todoscuartel[1] = 1 Then
	li_Cuartel = -1
Else
	li_Cuartel  = dw_1.Object.cuartel[1]
	If IsNull(li_Cuartel ) Then
		MessageBox( "Cuartel Erróneo", "Falta seleccionar un Cuartel.", StopSign!, Ok!)
		Return				 
   End If
End If

//Acepta condicion de mercado
If dw_1.Object.todosmercado[1] = 1 Then
	li_condmerc = -1
Else
	li_condmerc  = dw_1.Object.mercado[1]
	If IsNull(li_condmerc ) Then
		MessageBox( "Mercado Erróneo", "Falta seleccionar un Mercado.", StopSign!, Ok!)
		Return				 
   End If
End If

li_consfeccos	=	dw_1.Object.consfeccos[1]

If ii_recepciones = 1 Then ii_recepciones = 20

vinf.dw_1.SetTransObject(sqlca)

If rb_1.Checked Then
	ll_Fila	=	vinf.dw_1.Retrieve(li_Planta, ll_Frigo, ll_Camara, li_Especie,li_Variedad, &
										 ls_Tratamiento, ll_Productor, li_Categoria, ii_sentidomov, &
										 li_movto, ld_fechadesde, ld_fechahasta,0, li_exporta,&
										 li_Predio, li_Cuartel, li_condmerc,li_zona,li_consfeccos)
Else
	ll_Fila	=	vinf.dw_1.Retrieve(li_Planta, ll_Frigo, ll_Camara, li_Especie,li_Variedad, &
										 ls_Tratamiento, ll_Productor, li_Categoria, ii_sentidomov, &
										 li_movto, ld_fechadesde, ld_fechahasta,0, li_exporta, ii_recepciones,&
										 li_Predio, li_Cuartel, li_condmerc,li_zona)
End If

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy("t_titulo.text = '" + ls_titulo + "'")
	If dw_1.Object.totalespe[1] = 0 Then vinf.dw_1.ModIfy("DataWindow.Trailer.2.Height=0")
	If dw_1.Object.totalvari[1] = 0 Then vinf.dw_1.ModIfy("DataWindow.Trailer.3.Height=0")
	If dw_1.Object.totalprod[1] = 0 Then vinf.dw_1.ModIfy("DataWindow.Trailer.4.Height=0")
	If dw_1.Object.totaltrat[1] = 0 Then vinf.dw_1.ModIfy("DataWindow.Trailer.5.Height=0")
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	Commit;
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_recepcion_movimiento_diario
integer x = 2651
integer y = 968
integer taborder = 380
end type

type dw_1 from datawindow within w_info_recepcion_movimiento_diario
integer x = 247
integer y = 472
integer width = 2235
integer height = 1452
integer taborder = 380
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_recepcion_diaria_seleccion"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null, li_cliente
String	ls_Columna

SetNull(li_Null)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	Case "cliente"
		If Not iuo_Cliente.Existe(Integer(data),True,Sqlca) Then
			This.SetItem(Row, ls_Columna, li_Null )
			Return 1
		End If
		
	Case  "todos" 
		This.Object.cliente[Row] 	= 	li_Null
		
	CASE "planta"
		IF NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) THEN
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			RETURN 1
		ElSE
			dw_1.Object.todosfrigo.protect = 0
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
		IF Not iuo_Camarasfrigo.Existe(dw_1.Object.Planta[1], Integer(data), True, Sqlca) THEN
			This.SetItem(1, "frigo", li_Null )
			This.SetFocus()
			RETURN 1
		ElSE
			dw_1.Object.todoscamara.protect = 0
		END IF		

		idwc_camaras.SetFilter("cama_codfri = " + data + " And cama_codfri <> cama_codigo") 
		idwc_camaras.Filter()

	CASE "camara"
		IF NOT iuo_CamarasFrigo.Existe(dw_1.Object.Planta[1],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "camara", li_Null )
			This.SetFocus()
			RETURN 1
		END IF		
		

	CASE "especie"
		This.Object.Variedad[row]	=	li_Null
		IF NOT iuo_Especie.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "especie", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			idwc_variedad.settransobject(sqlca)
			idwc_variedad.Retrieve(Integer(data))
		END IF
	
  CASE	"todosespe"
		IF	data = '1' THEN
			This.Object.especie[row]		=	li_Null
			this.Object.variedad[row]		=	li_Null
			dw_1.Object.todosvari[row]		=	1
		ELSE
			This.Object.especie[row]		=	li_Null
			This.Object.Variedad[row]		=	li_Null
		END IF
		
	CASE "solorecepciones"
	ii_recepciones = Integer(data)

	CASE "variedad"
	
		IF NOT iuo_Variedades.Existe(This.Object.especie[row],Integer(data),True,SqlCa) THEN
			This.SetItem(1, "variedad", li_Null )
			This.SetFocus()
			RETURN 1
		END IF
		
	CASE "todosvari"
		IF	data = '1' THEN
			dw_1.Object.variedad[row]		=	li_Null
		END IF
		
	CASE "tratamiento"
		IF NOT iuo_TratamientoFrio.ofp_Recupera_TratamientoFrio(SqlCa,data,True) THEN
			This.SetItem(1, "tratamiento", String(li_Null))
			This.SetFocus()
			RETURN 1
		END IF

 		
	CASE "categoria"
		IF NOT iuo_Categorias.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "categoria", li_Null)
			This.SetFocus()
			RETURN 1
		END IF
	CASE "zona"
		IF NOT iuo_zonas.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "zona", li_Null)
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
		IF NOT iuo_ProdPredio.Existe(Integer(data),This.Object.productor[Row],True,SQLCA) THEN
			This.Object.predio[Row]	=	li_null
			RETURN 1
		ELSE		
			idwc_Cuartel.Retrieve(This.Object.productor[Row],Integer(data))
			idwc_certificacion.Retrieve(This.Object.productor[Row],Integer(data),This.Object.especie[row])
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
			dw_1.Object.todosfrigo.protect = 0
		ELSEIF data = '1' THEN 
			This.Object.planta[row]			=	li_Null
			dw_1.Object.frigo[row]			=	li_Null
			dw_1.Object.todosfrigo[row]	=	1
			dw_1.Object.camara[row]			=  li_null
			dw_1.Object.todoscamara[row]  =	1
		END IF

	CASE "todosfrigo"
		IF data='0' THEN
			dw_1.Object.todoscamara.protect = 0
		ELSEIF data = '1' THEN 
			This.Object.frigo[row]	=	li_Null
			dw_1.Object.camara[row]		=	li_Null
			dw_1.Object.todoscamara[row] = 1	
		End IF

	CASE "todoscama"
		IF data='0' THEN 
			dw_1.Object.todoscama.protect = 1
		ELSEIF data = '1' THEN 
			This.Object.camara[row]	= li_Null
		END IF
		
	
	CASE	"todosmovto"
		IF	data = '1' THEN This.Object.movimiento[row]	=	li_Null
	
	CASE	"todostrata"
		IF	data = '1' THEN This.Object.tratamiento[row]	=	String(li_Null)
	
	CASE	"todoscate"
		IF	data = '1' THEN This.Object.categoria[row]	=	li_Null
			
	CASE	"todosprod"
		dw_1.Object.TodosZona[row]			=	1
		dw_1.Object.TodosPredio[row]		=	1	
		dw_1.Object.TodosCuartel[row]		=	1
		dw_1.Object.TodosMercado[row]		=	1
		This.Object.Productor[row]			=	li_Null
		This.Object.Predio[row]				=	li_Null
		This.Object.Zona[row]				=	li_Null
		This.Object.Cuartel[row]			=	li_Null
		This.Object.Mercado[row]			=	li_Null
		
	CASE "Todoszona"
		IF Data = '1' THEN This.Object.Zona[row]				=	li_Null
		
	CASE "Todoscuartel"
		IF Data = '1' THEN This.Object.Cuartel[row]			=	li_Null
		
	CASE "Todosmercado"
		IF Data = '1' THEN This.Object.Mercado[row]			=	li_Null
		
	CASE "todospredio"
		IF Data = '1' THEN
			This.Object.Cuartel[row]			=	li_Null
			This.Object.Mercado[row]			=	li_Null
			dw_1.Object.TodosCuartel[row]		=	1
			dw_1.Object.TodosMercado[row]		=	1
		END IF
		
	CASE  "consfrigo"
		IF data = '1' THEN
			dw_1.Object.conscamara[row] = 1
		ELSE
			dw_1.Object.conscamara[row] = 0
		END IF

	CASE  "movimiento"
		IF NOT iuo_tipomovtofruta.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "categoria", li_Null)
			This.SetFocus()
			RETURN 1
		END IF
		
	
END CHOOSE
end event

event itemerror;Return 1
end event

type rb_1 from radiobutton within w_info_recepcion_movimiento_diario
integer x = 247
integer y = 388
integer width = 663
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 16777215
string text = "Planta/Fecha/Especie"
boolean lefttext = true
end type

event clicked;dw_1.Object.solorecepciones.Visible = FALSE
end event

type rb_2 from radiobutton within w_info_recepcion_movimiento_diario
integer x = 960
integer y = 388
integer width = 1522
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 16777215
string text = "Planta/Fecha/Guía/Especie - Con Movimientos de Bins"
boolean lefttext = true
end type

event clicked;dw_1.Object.solorecepciones.Visible = TRUE
ii_recepciones = 1
dw_1.Object.solorecepciones[1] = 1
end event

