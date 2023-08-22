$PBExportHeader$w_info_existencia_fruta_comercial.srw
$PBExportComments$Ventana de Informe Existencias de Fruta Comercial
forward
global type w_info_existencia_fruta_comercial from w_para_informes
end type
type dw_1 from datawindow within w_info_existencia_fruta_comercial
end type
type rb_1 from radiobutton within w_info_existencia_fruta_comercial
end type
type rb_2 from radiobutton within w_info_existencia_fruta_comercial
end type
type st_1 from statictext within w_info_existencia_fruta_comercial
end type
type rb_3 from radiobutton within w_info_existencia_fruta_comercial
end type
end forward

global type w_info_existencia_fruta_comercial from w_para_informes
integer x = 14
integer y = 32
integer width = 3063
integer height = 2084
string title = "Existencia Fruta Comercial"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_1 dw_1
rb_1 rb_1
rb_2 rb_2
st_1 st_1
rb_3 rb_3
end type
global w_info_existencia_fruta_comercial w_info_existencia_fruta_comercial

type variables
uo_plantadesp			iuo_plantadesp
uo_camarasbode			iuo_camarasbode
uo_especie				iuo_especie
uo_grupoespecie		iuo_grupoespecie
uo_subgrupoespecie	iuo_subgrupoespecie
uo_variedades			iuo_variedades
uo_tratamientofrio	iuo_tratamientofrio
uo_periodofrio			iuo_periodofrio                                                       
uo_categorias			iuo_categorias
uo_productores			iuo_productores


DataWindowChild	idwc_planta, idwc_frigorifico, idwc_categorias, idwc_camaras, idwc_especie, &
						idwc_grupo, idwc_subgrupo, idwc_variedad, idwc_tratamiento, idwc_periodo, &
						idwc_productor, idwc_exportador

String	is_NomPlanta
Integer	ii_Planta, ii_Frigorifico, ii_Especie, ii_Grupo
end variables

on w_info_existencia_fruta_comercial.create
int iCurrent
call super::create
this.dw_1=create dw_1
this.rb_1=create rb_1
this.rb_2=create rb_2
this.st_1=create st_1
this.rb_3=create rb_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
this.Control[iCurrent+2]=this.rb_1
this.Control[iCurrent+3]=this.rb_2
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.rb_3
end on

on w_info_existencia_fruta_comercial.destroy
call super::destroy
destroy(this.dw_1)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.st_1)
destroy(this.rb_3)
end on

event open;call super::open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp
iuo_camarasbode		=	Create uo_camarasbode
iuo_especie				=	Create uo_especie
iuo_grupoespecie		=	Create uo_grupoespecie
iuo_subgrupoespecie	=	Create uo_subgrupoespecie
iuo_variedades			=	Create uo_variedades
iuo_tratamientofrio	=	Create uo_tratamientofrio
iuo_periodofrio			=	Create uo_periodofrio
iuo_categorias			=	Create uo_categorias
iuo_productores		=	Create uo_productores

//Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
If idwc_planta.Retrieve() = 0 Then
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
End If

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
If idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 Then
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
End If

//Variedad
dw_1.GetChild("variedad",idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
idwc_variedad.InsertRow(0)

//Grupo
dw_1.GetChild("grupo",idwc_Subgrupo)
idwc_Subgrupo.SetTransObject(Sqlca)
If idwc_Subgrupo.Retrieve(0,0) = 0 Then
	MessageBox("Atención","Falta Registrar Grupos")
	idwc_Subgrupo.InsertRow(0)
End If

//Sub Grupo
dw_1.GetChild("subgrupo",idwc_grupo)
idwc_grupo.SetTransObject(Sqlca)
If idwc_grupo.Retrieve(0,0) = 0 Then
	MessageBox("Atención","Falta Registrar Sub Grupos")
	idwc_grupo.InsertRow(0)
End If

//Tratamiento
dw_1.GetChild("tratamiento",idwc_tratamiento)
idwc_tratamiento.SetTransObject(Sqlca)
If idwc_tratamiento.Retrieve() = 0 Then
	MessageBox("Atención","Falta Registrar Tratamientos")
	idwc_tratamiento.InsertRow(0)
End If

//Periodo
dw_1.GetChild("periodo",idwc_periodo)
idwc_periodo.SetTransObject(Sqlca)
If idwc_periodo.Retrieve() = 0 Then
	MessageBox("Atención","Falta Registrar Periodos")
	idwc_periodo.InsertRow(0)
End If

//Categorias
dw_1.GetChild("categoria",idwc_categorias)
idwc_categorias.SetTransObject(Sqlca)
If idwc_periodo.Retrieve() = 0 Then
	MessageBox("Atención","Falta Registrar Categorias")
	idwc_categorias.InsertRow(0)
End If
idwc_categorias.Setfilter('isNull( cate_embala ) or  cate_embala <> 1')
idwc_categorias.Filter()

//Productor
dw_1.GetChild("productor",idwc_productor)
idwc_productor.SetTransObject(Sqlca)
If idwc_productor.Retrieve(-1) = 0 Then
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
End If

//Exportador
dw_1.GetChild("cliente",idwc_exportador)
idwc_exportador.SetTransObject(Sqlca)
If idwc_exportador.Retrieve() = 0 Then
	MessageBox("Atención","Falta Registrar Exportador")
	idwc_Exportador.InsertRow(0)
End If

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

dw_1.setitem(1,"cliente", gi_codexport)
dw_1.setitem(1,"tipopool",2)

end event

type pb_excel from w_para_informes`pb_excel within w_info_existencia_fruta_comercial
integer x = 2688
integer y = 372
end type

type st_computador from w_para_informes`st_computador within w_info_existencia_fruta_comercial
integer x = 2075
end type

type st_usuario from w_para_informes`st_usuario within w_info_existencia_fruta_comercial
integer x = 2075
end type

type st_temporada from w_para_informes`st_temporada within w_info_existencia_fruta_comercial
integer x = 2075
end type

type p_logo from w_para_informes`p_logo within w_info_existencia_fruta_comercial
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_existencia_fruta_comercial
integer width = 2350
string text = "Existencia de Fruta Comercial"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existencia_fruta_comercial
integer x = 2688
integer y = 720
integer taborder = 130
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	li_Planta, li_FrigorIfico, li_Especie, li_Grupo, li_SubGrupo, li_exportador, li_tipool, &
			li_Variedad, li_Periodo, li_Categoria, li_NroLote, li_ConsLote,rb_espe,rb_cam, li_estado
Long		ll_Fila, ll_Productor, ll_Camara
String	ls_Tratamiento, ls_Consgcalib

dw_1.AcceptText()

istr_info.titulo	= 'EXISTENCIA DE FRUTA COMERCIAL'

OpenWithParm(vinf, istr_info)

If	rb_1.Checked Then
	vinf.dw_1.DataObject = "dw_info_existencia_frutacomercial"
ElseIf rb_2.Checked Then
	vinf.dw_1.DataObject = "dw_info_existencia_frutacomercial_camara"
ElseIf rb_3.Checked Then
	vinf.dw_1.DataObject = "dw_info_existencia_frutacomercial_bins"
End If

// Acepta Exportador
li_Exportador = dw_1.Object.cliente[1]
If IsNull(li_Exportador) Then
	MessageBox( "Exportador Erróneo", "Falta seleccionar un Exportador.", &
             StopSign!, Ok!)
	Return 1				 
End If

// Acepta Planta //
If dw_1.Object.todosplanta[1] = 1 Then
	li_Planta = 10000
Else
	li_Planta = dw_1.Object.planta[1]
	If IsNull(li_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", &
	             StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta FrigorIfico //
If dw_1.Object.todosfrigo[1] = 1 Then
	li_FrigorIfico = 10000
Else
	li_FrigorIfico = dw_1.Object.frigorIfico[1]
	If IsNull(li_FrigorIfico) Then
		MessageBox( "FrigorIfico Erróneo", "Falta seleccionar un Frigorífico.", &
	             StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Camara //
If dw_1.Object.todoscama[1] = 1 Then
	ll_Camara = 10000
	If dw_1.Object.conscama[1] =1 Then	
		ll_Camara	=	90000
	End If
Else
	ll_Camara	=	dw_1.Object.camara[1]
	If IsNull(ll_Camara) Then
		MessageBox( "Camara Errónea", "Falta seleccionar una Camara.", &
	             StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Especie //
If dw_1.Object.todosespe[1] = 1 Then
	li_Especie = 100
Else
	li_Especie = dw_1.Object.especie[1]
	If IsNull(li_especie) Then
		MessageBox( "Especie Errónea", "Falta seleccionar una Especie.", &
	             StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Variedad //
If dw_1.Object.todosvari[1] = 1 Then
	li_Variedad = 10000
Else
	li_Variedad = dw_1.Object.variedad[1]
	If IsNull(li_Variedad) Then
		MessageBox( "Variedad Errónea", "Falta seleccionar una Variedad.", &
	             StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Grupo de Variedades //
If dw_1.Object.todosvari[1] = 1 Then  
	If dw_1.Object.todosgrupo[1] = 1  Then
		li_Grupo = 100
		If dw_1.Object.consgrupo[1] = 1  Then	
			li_Grupo =	900
		End If
	Else
		li_Grupo = dw_1.Object.grupo[1]
		If IsNull(li_Grupo) Then
			MessageBox( "Grupo Erróneo", "Falta seleccionar un Grupo de Especie.", &
	      	       StopSign!, Ok!)
			Return 1				 
   	End If
	End If
Else
	li_Grupo = 100
End If

// Acepta SubGrupo de Variedades //
If dw_1.Object.todosvari[1] = 1 Then 
	If dw_1.Object.todossubgru[1] = 1  Then
		li_SubGrupo = 100
		If dw_1.Object.conssubgr[1] = 1 Then
			li_SubGrupo=900
		End If
	Else			
		li_SubGrupo = dw_1.Object.subgrupo[1]
		If IsNull(li_SubGrupo) Then
			MessageBox( "SubGrupo Erróneo", "Falta seleccionar un SubGrupo de Especie.", &
	  	          	 StopSign!, Ok!)
			Return 1				 
    	End If
	End If
Else
	li_SubGrupo = 100
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
		MessageBox( "Tratamiento Erróneo", "Falta seleccionar un Tratamiento.", &
	             StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Periodo de Frio //
If dw_1.Object.todosperio[1] = 1 Then
	li_Periodo = 100
	If dw_1.Object.consperio[1] = 1 Then	
		li_Periodo = 900
	End If
Else
	li_Periodo = dw_1.Object.periodo[1]
	If IsNull(li_Periodo) Then
		MessageBox( "Período Erróneo", "Falta seleccionar un Período.", &
	             StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Categorias
If dw_1.Object.todoscate[1] = 1  Then
	li_Categoria = 1000
Else
	li_Categoria = dw_1.Object.categoria[1]
	If IsNull(li_Categoria) Then
		MessageBox( "Categoria Errónea", "Falta seleccionar una Categoria.", &
	             StopSign!, Ok!)
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
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.", &
	             StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Tipo Pool
li_tipool = dw_1.Object.tipopool[1]

li_estado = dw_1.Object.estado[1]

// Acepta Todos Lotes o Consolida
If dw_1.Object.consollote[1] = 1 Then
		li_ConsLote =	1
	Else
		li_ConsLote = 0
End If

// Acepta Todos Grupos Calibres ó Consolidados
If dw_1.Object.consgcalib[1] = 0 Then
		ls_Consgcalib =	'*'
	Else
		ls_Consgcalib = '**'
End If

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_Planta,li_FrigorIfico,ll_Camara,li_Especie,li_Grupo,&
										 li_SubGrupo,li_Variedad,ls_Tratamiento,li_Periodo,li_Categoria,&
										 ll_Productor,li_ConsLote, ls_Consgcalib,li_exportador,li_tipool, li_estado)
If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
   F_Membrete(vinf.dw_1)
	If dw_1.Object.totalespe[1] = 0 Then vinf.dw_1.ModIfy("DataWindow.Trailer.2.Height=0")
	If dw_1.Object.totalgrup[1] = 0 Then vinf.dw_1.ModIfy("DataWindow.Trailer.3.Height=0")
	If dw_1.Object.totalsubg[1] = 0 Then vinf.dw_1.ModIfy("DataWindow.Trailer.4.Height=0")
	If dw_1.Object.totalvari[1] = 0 Then vinf.dw_1.ModIfy("DataWindow.Trailer.5.Height=0")
	If dw_1.Object.totaltrat[1] = 0 Then vinf.dw_1.ModIfy("DataWindow.Trailer.6.Height=0")
	If dw_1.Object.totalperi[1] = 0 Then vinf.dw_1.ModIfy("DataWindow.Trailer.7.Height=0")
	If dw_1.Object.totalgcal[1] = 0 Then vinf.dw_1.ModIfy("DataWindow.Trailer.8.Height=0")
	If dw_1.Object.totalprod[1] = 0 Then vinf.dw_1.ModIfy("DataWindow.Trailer.9.Height=0")
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_existencia_fruta_comercial
integer x = 2688
integer y = 1008
integer taborder = 140
end type

type dw_1 from datawindow within w_info_existencia_fruta_comercial
integer x = 256
integer y = 468
integer width = 2286
integer height = 1416
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_existencia_camara_seleccion_com"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null
String	ls_Columna

SetNull(li_Null)

ls_Columna	=	dwo.Name

CHOOSE Case ls_Columna
	Case "planta"
		If NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) Then
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			Return 1
		Else
			dw_1.Object.todosfrigo.protect = 0
		End If		
		
		If idwc_frigorifico.Retrieve(Integer(data)) = 0 Then
			MessageBox("Atención","Falta Registrar Frigorífico asociados a Planta Administradora")
			Return 1
		Else
			idwc_frigorifico.SetFilter("cama_codfri = cama_codigo")
			idwc_frigorifico.Filter()
		End If
	
		If idwc_camaras.Retrieve(Integer(data)) = 0 Then
			MessageBox("Atención","Falta Registrar Cámaras Frigorífico Seleccionado")
			Return 1
		End If
		
	Case "frigorifico"
		
		If NOT iuo_Camarasbode.Existe(This.Object.planta[1],Integer(data),True,Sqlca) Then
			This.SetItem(1, "frigorifico", li_Null )
			This.SetFocus()
			Return 1
		ElSE
			dw_1.Object.todoscama.protect = 0
		End If
		
		idwc_camaras.SetFilter("cama_codfri = " + data + " And cama_codfri <> cama_codigo") 
		idwc_camaras.Filter()
		
	Case "camara"
		If NOT iuo_Camarasbode.Existe(This.Object.planta[1],Integer(data),True,SqlCa) Then
			This.SetItem(1, "camara", li_Null )
			This.SetFocus()
			Return 1
		Else
			dw_1.Object.todoscama.protect = 0
		End If

	Case "especie"
		
		If NOT iuo_Especie.Existe(Integer(data),True,SqlCa) Then
			This.SetItem(1, "especie", li_Null )
			This.SetFocus()
			Return 1
		Else
			idwc_grupo.Retrieve(Integer(data),0)
			This.Object.Grupo[1]				=	li_Null
			This.Object.todosgrupo.Protect	=	0
			/**/
			idwc_subgrupo.Retrieve(Integer(data),0)
			This.Object.SubGrupo[1]			=	li_Null
			This.Object.todosSubGru.Protect	=	0			
			/**/
			idwc_variedad.Retrieve(Integer(data),gstr_parempresa.empr_codexp)
			This.Object.Variedad[1]		=	li_Null
		End If

	Case "variedad"
		
		If NOT iuo_Variedades.Existe(This.Object.especie[1],Integer(data),True,SqlCa) Then
			This.SetItem(1, "variedad", li_Null )
			This.SetFocus()
			Return 1
		Else
			This.Object.Grupo[1]			=	iuo_Variedades.Grupo
			This.Object.SubGrupo[1]		=	iuo_Variedades.SubGrupo
			
			This.Object.todosgrupo.Protect	=	1
			This.Object.todossubgru.Protect	=	1
		End If
		
	Case "grupo"
		If NOT iuo_GrupoEspecie.Existe(This.Object.especie[1],Integer(data),True,SqlCa) Then
			This.SetItem(1, "grupo", li_Null )
			This.SetFocus()
			Return 1
		Else
			idwc_grupo.Retrieve(This.Object.especie[1],Integer(data))
			This.Object.Grupo[1]	=	li_Null
		End If
			
	Case "subgrupo"
		If NOT iuo_SubGrupoEspecie.Existe(This.Object.especie[1],&
					This.Object.grupo[1],Integer(data),True,SqlCa) Then
			This.SetItem(1, "subgrupo", li_Null )
			This.SetFocus()
			Return 1
		End If

	Case "tratamiento"
		If NOT iuo_TratamientoFrio.Ofp_Recupera_TratamientoFrio(SqlCa,data,True) Then
			This.SetItem(1, "tratamiento", String(li_Null))
			This.SetFocus()
			Return 1
		End If

	Case "periodo"
		If NOT iuo_PeriodoFrio.Ofp_Recupera_PeriodoFrio(SqlCa,Integer(data),True) Then
			This.SetItem(1, "periodo", li_Null)
			This.SetFocus()
			Return 1
		End If
		
	Case "categoria"
		If NOT iuo_Categorias.Existe(Integer(data),True,SqlCa) Then
			This.SetItem(1, "categoria", li_Null)
			This.SetFocus()
			Return 1
		End If

	Case "productor"
		If NOT iuo_Productores.Existe(long(data),True,SqlCa) Then
			This.SetItem(1, "productor", li_Null)
			This.SetFocus()
			Return 1
		End If

	Case "todosplanta"
		If data='0' Then
			dw_1.Object.todosfrigo.protect = 1
		Else
		If data = '1' Then This.Object.planta[1]		=	li_Null
			dw_1.Object.frigorifico[1]		= li_Null
			dw_1.Object.todosfrigo.protect= 1
			dw_1.Object.todosfrigo[1]		= 1
			dw_1.Object.todoscama.protect	= 1
			dw_1.Object.camara[1]			= li_Null
			dw_1.Object.todoscama[1]		= 1
		End If

	Case "todosfrigo"
		If data='0' Then
			dw_1.Object.todoscama.protect = 0
		Else
		If data = '1' Then This.Object.frigorifico[1]	=	li_Null
			dw_1.Object.todoscama.protect = 1
			dw_1.Object.camara[1]			= li_Null
			dw_1.Object.todoscama[1] 		= 1	
		End If

	Case "todoscama"
		If data='0' Then 
			dw_1.Object.todoscama.protect = 1
		Else
		If data = '1' Then This.Object.camara[1]	= li_Null
			dw_1.Object.camara[1]	 =	li_Null
			dw_1.Object.todoscama[1] =	1
		End If
		
	Case	"todosespe"
		If	data = '1' Then This.Object.especie[1]	=	li_Null
			dw_1.Object.variedad[1]		= li_Null
			dw_1.Object.todosvari[1]	= 1
			This.SetItem(1, "subgrupo", li_Null )
			This.SetItem(1, "grupo", li_Null )
			
	Case	"todosvari"
		If	data = '1' Then 
			This.Object.variedad[1]				= li_Null
			dw_1.Object.todosvari[1]			=	1
			This.Object.todosgrupo.Protect	=	0
			This.Object.todossubgru.Protect	=	0
			This.SetItem(1, "subgrupo", li_Null )
			This.SetItem(1, "grupo", li_Null )	
		Else	
			This.Object.todosgrupo.Protect	=	1
			This.Object.todossubgru.Protect	=	1
		End If	
		
	Case	"todosgrupo"
		If	data = '1' Then 
			This.Object.todossubgru.Protect	= 1
			dw_1.Object.todossubgru[1]			= 1			
			This.SetItem(1, "subgrupo", li_Null )
			This.SetItem(1, "grupo", li_Null )	
		Else	
			This.Object.todossubgru.Protect	=	0
		End If	
				
	Case	"todostrata"
		If	data = '1' Then This.Object.tratamiento[1]	=	String(li_Null)
	
	Case	"todosperio"
		If	data = '1' Then This.Object.periodo[1]		=	li_Null
	
	Case	"todoscate"
		If	data = '1' Then This.Object.categoria[1]	=	li_Null
			
	Case	"todosprod"
		If	data = '1' Then This.Object.productor[1]	=	li_Null
		Return 0
End CHOOSE
end event

event itemerror;RETURN 1
end event

type rb_1 from radiobutton within w_info_existencia_fruta_comercial
integer x = 896
integer y = 396
integer width = 535
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 8388608
long backcolor = 553648127
string text = "Categoria/Especie"
boolean checked = true
boolean lefttext = true
boolean righttoleft = true
end type

type rb_2 from radiobutton within w_info_existencia_fruta_comercial
integer x = 1518
integer y = 396
integer width = 480
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 8388608
long backcolor = 553648127
string text = "Camara/Cat/Esp."
boolean lefttext = true
end type

type st_1 from statictext within w_info_existencia_fruta_comercial
integer x = 261
integer y = 408
integer width = 603
integer height = 56
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 8388608
long backcolor = 553648127
string text = "Informe Ordenado Por :"
long bordercolor = 12632256
boolean focusrectangle = false
end type

type rb_3 from radiobutton within w_info_existencia_fruta_comercial
integer x = 2057
integer y = 396
integer width = 443
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 8388608
long backcolor = 553648127
string text = "Cat/Esp/Tarjas"
boolean lefttext = true
end type

